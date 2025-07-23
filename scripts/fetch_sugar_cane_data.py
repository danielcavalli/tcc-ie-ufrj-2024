#!/usr/bin/env python
"""Fetch sugar-cane production and climate panel from Base dos Dados.

Usage
-----
$ python scripts/fetch_sugar_cane_data.py --project_id YOUR_GCP_PROJECT \
        --destination data/csv/PAM_MET_sugar_cane_fresh.csv

The script requires the `basedosdados` package (``pip install basedosdados``)
with credentials configured for the supplied ``--project_id`` as described in
https://basedosdados.github.io/mais/access_data_bq/#credenciais.
"""

# stdlib
import argparse
import pathlib
import sys

# third-party
import pandas as pd  # type: ignore
import basedosdados as bd  # type: ignore


DEFAULT_SQL_PATH = pathlib.Path("data/sql_query/BD_BigQuery_SC.sql")


def load_sql(sql_path: pathlib.Path) -> str:
    """Return the contents of the SQL file, stripping any leading BOM."""
    if not sql_path.exists():
        sys.exit(f"SQL file not found: {sql_path}")
    text = sql_path.read_text(encoding="utf-8")
    return text.lstrip("\ufeff")  # remove BOM if present


def run_query(sql: str, project_id: str) -> pd.DataFrame:
    """Execute *sql* on BigQuery using basedosdados and return a DataFrame."""
    print("Running query on BigQuery – this may take a while …")
    df = bd.read_sql(query=sql, billing_project_id=project_id)
    print(f"Returned {len(df):,} rows.")
    return df


def save(df: pd.DataFrame, destination: pathlib.Path) -> None:
    """Save *df* to *destination* (CSV or Parquet based on suffix)."""
    destination.parent.mkdir(parents=True, exist_ok=True)
    if destination.suffix == ".parquet":
        df.to_parquet(destination, index=False)
    elif destination.suffix == ".csv":
        df.to_csv(destination, index=False)
    else:
        sys.exit("Destination must end with .csv or .parquet")
    print(f"Saved results to {destination}")


def main() -> None:
    parser = argparse.ArgumentParser(description="Fetch sugar-cane panel from BD")
    parser.add_argument("--project_id", required=True, help="GCP billing project id")
    parser.add_argument(
        "--sql_path",
        type=pathlib.Path,
        default=DEFAULT_SQL_PATH,
        help="Path to the SQL file to execute",
    )
    parser.add_argument(
        "--destination",
        type=pathlib.Path,
        default=pathlib.Path("data/csv/sugar_cane_panel.csv"),
        help="Where to store the resulting table (supports .csv or .parquet)",
    )
    args = parser.parse_args()

    sql = load_sql(args.sql_path)
    df = run_query(sql, project_id=args.project_id)
    save(df, destination=args.destination)


if __name__ == "__main__":
    main()
