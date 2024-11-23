import pandas as pd
import numpy as np
from sklearn.preprocessing import StandardScaler
from sklearn.linear_model import LogisticRegression
from sklearn.impute import SimpleImputer
import scipy.stats as stats
import logging
from datetime import datetime

class PSMatcher:
    def __init__(self, caliper=0.2, n_matches=7):
        self.caliper = caliper
        self.n_matches = n_matches
        self.setup_logging()
        
    def setup_logging(self):
        """Configure logging for the matching process."""
        logging.basicConfig(
            level=logging.INFO,
            format='%(asctime)s - %(levelname)s - %(message)s',
            filename=f'psm_matching_{datetime.now().strftime("%Y%m%d_%H%M%S")}.log'
        )
        
    def prepare_data(self, df):
        """Enhanced data preparation specifically for the agricultural dataset."""
        logging.info("Starting data preparation")
        df_clean = df.copy()
        
        # Create lists of column names by type
        col_types = {
            'precip': [col for col in df.columns if 'precipitacao' in col],
            'pressao': [col for col in df.columns if 'pressao' in col],
            'temp': [col for col in df.columns if 'temp_max' in col],
            'umidade': [col for col in df.columns if 'umidade' in col],
            'vento': [col for col in df.columns if 'vento' in col]
        }
        
        # Calculate pre-treatment averages
        df_clean['avg_pre_production'] = df_clean.groupby('id_microrregiao')['total_valor_producao'].transform(
            lambda x: x.expanding().mean()
        )
        
        # Handle missing values by type
        imputer = SimpleImputer(strategy='median')
        for cols in col_types.values():
            if cols:
                df_clean[cols] = imputer.fit_transform(df_clean[cols])
        
        # Calculate yearly averages with outlier capping
        for prefix, cols in col_types.items():
            if cols:
                df_clean[f'avg_{prefix}'] = df_clean[cols].mean(axis=1)
                lower = np.percentile(df_clean[f'avg_{prefix}'], 1)
                upper = np.percentile(df_clean[f'avg_{prefix}'], 99)
                df_clean[f'avg_{prefix}'] = df_clean[f'avg_{prefix}'].clip(lower, upper)
        
        # Add year and region effects
        df_clean['year_effect'] = df_clean.groupby('ano')['total_valor_producao'].transform('mean')
        df_clean['region_trend'] = df_clean.groupby('id_microrregiao').cumcount()
        
        df_clean.dropna(inplace=True)
        logging.info(f"Data preparation complete. Rows remaining: {len(df_clean)}")
        
        return df_clean
    
    def get_treatment_events(self, df):
        """Identify all treatment events."""
        logging.info("Identifying treatment events")
        treatment_events = []
        treated_regions = df[df['treatment'] == 1]['id_microrregiao'].unique()
        
        for region in treated_regions:
            region_data = df[df['id_microrregiao'] == region].sort_values('ano')
            treatment_year = region_data[region_data['treatment'] == 1]['ano'].min()
            
            treatment_events.append({
                'region': region,
                'treatment_year': treatment_year
            })
        
        logging.info(f"Found {len(treatment_events)} treatment events")
        return pd.DataFrame(treatment_events)
    
    def match_for_treatment_event(self, df, treated_region, treatment_year, features):
        """Perform matching for a specific treatment event."""
        df_period = df[df['ano'] <= treatment_year].copy()
        
        # Create treatment indicator
        df_period['current_treatment'] = ((df_period['id_microrregiao'] == treated_region) & 
                                        (df_period['ano'] == treatment_year)).astype(int)
        
        # Get potential controls
        potential_controls = df_period[
            (df_period['id_microrregiao'] != treated_region) & 
            (df_period['ano'].between(treatment_year - 4, treatment_year)) &
            (df_period['treatment'] == 0)
        ]
        
        treated_obs = df_period[df_period['current_treatment'] == 1]
        
        if len(treated_obs) == 0 or len(potential_controls) == 0:
            return None
        
        # Calculate propensity scores
        matching_data = pd.concat([treated_obs, potential_controls])
        X = StandardScaler().fit_transform(matching_data[features])
        
        model = LogisticRegression(random_state=42, class_weight='balanced')
        model.fit(X, matching_data['current_treatment'])
        propensity_scores = model.predict_proba(X)[:, 1]
        
        matching_data['propensity_score'] = propensity_scores
        treated_score = matching_data[matching_data['current_treatment'] == 1]['propensity_score'].iloc[0]
        
        # Find matches
        potential_controls = matching_data[matching_data['current_treatment'] == 0].copy()
        potential_controls['distance'] = abs(potential_controls['propensity_score'] - treated_score)
        
        # Apply caliper
        caliper_threshold = self.caliper * matching_data['propensity_score'].std()
        potential_controls = potential_controls[potential_controls['distance'] <= caliper_threshold]
        
        matches = potential_controls.nsmallest(n=self.n_matches, columns='distance')
        
        if len(matches) == 0:
            return None
        
        # Add matching information
        matches['treated_region'] = treated_region
        matches['treatment_year'] = treatment_year
        matches['matched_control'] = 1
        
        treated_obs['treated_region'] = treated_region
        treated_obs['treatment_year'] = treatment_year
        treated_obs['matched_control'] = 0
        
        return pd.concat([treated_obs, matches])
    
    def perform_matching(self, df):
        """Main matching function that coordinates the entire process."""
        logging.info("Starting matching process")
        
        # Prepare data
        df_clean = self.prepare_data(df)
        
        # Define features for matching
        features = [
            'avg_precip', 'avg_pressao', 'avg_temp', 'avg_umidade', 'avg_vento',
            'avg_pre_production', 'year_effect', 'region_trend'
        ]
        
        # Get treatment events
        treatment_events = self.get_treatment_events(df_clean)
        
        # Perform matching
        all_matches = []
        for _, event in treatment_events.iterrows():
            matches = self.match_for_treatment_event(
                df_clean,
                event['region'],
                event['treatment_year'],
                features
            )
            if matches is not None:
                all_matches.append(matches)
        
        if all_matches:
            matched_df = pd.concat(all_matches, ignore_index=True)
            matched_df['match_id'] = (matched_df['treated_region'].astype(str) + '_' + 
                                    matched_df['treatment_year'].astype(str))
            
            logging.info(f"Matching complete. Found {len(matched_df)} matched pairs")
            return matched_df
        else:
            logging.warning("No valid matches found")
            return None