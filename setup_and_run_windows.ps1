<#
.SYNOPSIS
    Automates the setup and execution of the R analysis project.
.DESCRIPTION
    This PowerShell script performs the following actions on a Windows machine:
    1. Checks and installs R if not present.
    2. Checks and installs Python if not present.
    3. Checks for renv.lock file.
    4. Sets up the R environment using renv.
    5. Verifies the existence of the required data file.
    6. Runs the main R analysis script.
    The script is designed to be run from the root of the project repository.
.NOTES
    Author: GitHub Copilot
    Date:   June 2, 2025
    Based on instructions from the project README.md.
#>

# Strict mode
Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop" # Exit on first error for critical operations

# --- Script Parameters (versions and paths from README) ---
$RVersion = "4.3.3"
$PythonVersion = "3.11.4"
$DataFilePathRelative = "data/csv/WightedMethod/sugar_cane_treated.csv"
$AnalysisScriptPathRelative = "notebooks/did_v2.r"
$RenvLockFileRelative = "renv.lock"

# Assume script is run from the repository root
$RepoRoot = Get-Location
$DataFilePath = Join-Path $RepoRoot.Path $DataFilePathRelative
$AnalysisScriptPath = Join-Path $RepoRoot.Path $AnalysisScriptPathRelative
$RenvLockPath = Join-Path $RepoRoot.Path $RenvLockFileRelative

# --- Helper Functions ---
function Test-CommandExists {
    param (
        [string]$CommandName
    )
    return (Get-Command $CommandName -ErrorAction SilentlyContinue) -ne $null
}

# --- Main Functions ---

function Install-R {
    Write-Host "INFO: Checking for R installation..."
    if (Test-CommandExists -CommandName "Rscript") {
        Write-Host "INFO: R (Rscript) is already installed and in PATH."
        return
    }

    Write-Host "INFO: R not found. Proceeding with installation of R version $RVersion."
    $RInstallerUrl = "https://cran.r-project.org/bin/windows/base/R-$RVersion-win.exe"
    $RInstallerPath = Join-Path $env:TEMP "R-$RVersion-win.exe"

    try {
        Write-Host "INFO: Downloading R $RVersion from $RInstallerUrl..."
        Invoke-WebRequest -Uri $RInstallerUrl -OutFile $RInstallerPath -UseBasicParsing
        Write-Host "INFO: Download complete."

        Write-Host "INFO: Installing R $RVersion silently..."
        # /SP- is used to suppress the "This will install..." prompt with /SILENT for Inno Setup installers
        $process = Start-Process -FilePath $RInstallerPath -ArgumentList "/SILENT", "/NORESTART", "/SP-" -Wait -PassThru
        if ($process.ExitCode -ne 0) {
            Write-Error "ERROR: R installation failed with exit code $($process.ExitCode)."
            exit 1 # Critical failure
        }
        Write-Host "INFO: R installation complete."

        Write-Host "INFO: Attempting to add R to PATH for the current session."
        $RBinPathX64 = "C:\Program Files\R\R-$RVersion\bin\x64"
        $RBinPathI386 = "C:\Program Files\R\R-$RVersion\bin\i386"

        if (Test-Path $RBinPathX64) {
            $env:PATH = "$RBinPathX64;$($env:PATH)"
            Write-Host "INFO: Added $RBinPathX64 to session PATH."
        } elseif (Test-Path $RBinPathI386) {
            $env:PATH = "$RBinPathI386;$($env:PATH)"
            Write-Host "INFO: Added $RBinPathI386 to session PATH."
        } else {
            Write-Warning "WARN: Could not find R installation in common paths (e.g., C:\Program Files\R\R-$RVersion\bin) to add to session PATH."
            Write-Warning "WARN: A permanent manual update to the system PATH might be required for R, or R might have installed to a non-standard location."
        }
        
        if (-not (Test-CommandExists -CommandName "Rscript")) {
            Write-Warning "WARN: Rscript is still not found in PATH after attempted addition. Manual PATH configuration may be needed, or a new terminal session."
        } else {
            Write-Host "INFO: Rscript is now available in PATH for this session."
        }
    }
    catch {
        Write-Error "ERROR: An error occurred during R installation: $($_.Exception.Message)"
        Write-Error "Exception details: $($_.ToString())"
        exit 1
    }
    finally {
        if (Test-Path $RInstallerPath) {
            Write-Host "INFO: Removing R installer: $RInstallerPath"
            Remove-Item $RInstallerPath -Force -ErrorAction SilentlyContinue
        }
    }
}

function Install-Python {
    Write-Host "INFO: Checking for Python installation..."
    if (Test-CommandExists -CommandName "python") {
        Write-Host "INFO: Python is already installed and in PATH."
        return
    }

    Write-Host "INFO: Python not found. Proceeding with installation of Python version $PythonVersion."
    $PythonInstallerUrl = "https://www.python.org/ftp/python/$PythonVersion/python-$PythonVersion-amd64.exe"
    $PythonInstallerPath = Join-Path $env:TEMP "python-$PythonVersion-amd64.exe"

    try {
        Write-Host "INFO: Downloading Python $PythonVersion from $PythonInstallerUrl..."
        Invoke-WebRequest -Uri $PythonInstallerUrl -OutFile $PythonInstallerPath -UseBasicParsing
        Write-Host "INFO: Download complete."

        Write-Host "INFO: Installing Python $PythonVersion silently..."
        # Arguments: /quiet InstallAllUsers=1 PrependPath=1 Include_test=0
        $process = Start-Process -FilePath $PythonInstallerPath -ArgumentList "/quiet InstallAllUsers=1 PrependPath=1 Include_test=0" -Wait -PassThru
        if ($process.ExitCode -ne 0) {
            Write-Error "ERROR: Python installation failed with exit code $($process.ExitCode)."
            exit 1 # Critical failure
        }
        Write-Host "INFO: Python installation complete."
        
        if (Test-CommandExists -CommandName "python") {
            Write-Host "INFO: Python is now available in PATH."
        } else {
            Write-Warning "WARN: Python was installed, but 'python' command is not yet available in PATH. A new terminal session or system restart might be required."
        }
    }
    catch {
        Write-Error "ERROR: An error occurred during Python installation: $($_.Exception.Message)"
        Write-Error "Exception details: $($_.ToString())"
        exit 1
    }
    finally {
        if (Test-Path $PythonInstallerPath) {
            Write-Host "INFO: Removing Python installer: $PythonInstallerPath"
            Remove-Item $PythonInstallerPath -Force -ErrorAction SilentlyContinue
        }
    }
}

function Check-RenvLockFile {
    Write-Host "INFO: Checking for renv.lock file at $RenvLockPath..."
    if (-not (Test-Path $RenvLockPath)) {
        Write-Error "ERROR: renv.lock file not found at $RenvLockPath. R environment cannot be restored."
        Write-Error "ERROR: The original author should run renv::init() or renv::snapshot() in R to create/update renv.lock."
        exit 1
    }
    Write-Host "INFO: renv.lock file found."
}

function Setup-REnvironment {
    Write-Host "INFO: Setting up R environment with renv..."
    
    if (-not (Test-CommandExists -CommandName "Rscript")) {
        Write-Error "ERROR: Rscript command not found. Cannot set up R environment. Please ensure R is installed and in PATH."
        exit 1
    }

    $RCommands = @'
# Ensure output is in English for easier parsing if needed
Sys.setenv(LANGUAGE = "en")

# 1. Install renv if it's not already available
if (!requireNamespace("renv", quietly = TRUE)) {
  writeLines("INFO: renv package not found. Installing renv...")
  install.packages("renv", repos = "https://cloud.r-project.org/", quiet = TRUE)
  writeLines("INFO: renv package installation attempted.")
} else {
  writeLines("INFO: renv package already available.")
}

# 2. Restore the project environment from renv.lock
writeLines("INFO: Attempting to restore R environment using renv::restore(). This may take some time...")
tryCatch({
    renv::restore(prompt = FALSE)
    writeLines("INFO: renv::restore() successfully completed.")
}, error = function(e) {
    writeLines(paste("ERROR: renv::restore() failed:", e$message))
    # Signal error to PowerShell by exiting R with non-zero status
    quit(save = "no", status = 1, runLast = FALSE)
})
'@

    $RenvSetupOutputLog = Join-Path $RepoRoot.Path "renv_setup_output.log"
    $RenvSetupErrorLog = Join-Path $RepoRoot.Path "renv_setup_error.log"

    Write-Host "INFO: Executing R commands for renv setup. Output will be logged to $RenvSetupOutputLog and $RenvSetupErrorLog."
    try {
        # Rscript inherits the current working directory ($RepoRoot)
        $process = Start-Process Rscript -ArgumentList "-e", $RCommands -Wait -NoNewWindow -PassThru -RedirectStandardOutput $RenvSetupOutputLog -RedirectStandardError $RenvSetupErrorLog
        
        $renvOutput = Get-Content $RenvSetupOutputLog -Raw -ErrorAction SilentlyContinue
        $renvError = Get-Content $RenvSetupErrorLog -Raw -ErrorAction SilentlyContinue

        if ($renvOutput) { Write-Host "INFO: Renv setup stdout (see $RenvSetupOutputLog for full details):`n$($renvOutput | Select-Object -First 5)`n..." }
        
        if ($process.ExitCode -ne 0) {
            Write-Error "ERROR: Renv setup script (Rscript) failed with exit code $($process.ExitCode)."
            if ($renvError) { Write-Error "ERROR: Renv setup stderr (see $RenvSetupErrorLog for full details):`n$renvError" }
            exit 1
        }
        if ($renvError) {
            Write-Warning "WARN: Renv setup produced messages on stderr (see $RenvSetupErrorLog for full details):`n$($renvError | Select-Object -First 5)`n..."
        }

        Write-Host "INFO: R environment setup with renv complete."
    }
    catch {
        Write-Error "ERROR: An error occurred during R environment setup (PowerShell try/catch): $($_.Exception.Message)"
        Write-Error "Exception details: $($_.ToString())"
        if (Test-Path $RenvSetupErrorLog) {
            $errContent = Get-Content $RenvSetupErrorLog -Raw -ErrorAction SilentlyContinue
            if ($errContent) { Write-Error "Renv setup error log content:`n$errContent" }
        }
        exit 1
    }
}

function Check-DataFile {
    Write-Host "INFO: Checking for required data file: $DataFilePath"
    if (-not (Test-Path $DataFilePath)) {
        Write-Error "ERROR: Data file not found at $DataFilePath. Analysis cannot proceed."
        exit 1
    }
    Write-Host "INFO: Data file found."
}

function Run-RAnalysis {
    Write-Host "INFO: Running the R analysis script: $AnalysisScriptPath"
    
    if (-not (Test-CommandExists -CommandName "Rscript")) {
        Write-Error "ERROR: Rscript command not found. Cannot run analysis. Please ensure R is installed and in PATH."
        exit 1
    }
    
    if (-not (Test-Path $AnalysisScriptPath)) {
        Write-Error "ERROR: Analysis script not found at $AnalysisScriptPath."
        exit 1
    }

    $AnalysisOutputLog = Join-Path $RepoRoot.Path "analysis_output.log"
    $AnalysisErrorLog = Join-Path $RepoRoot.Path "analysis_error.log"

    Write-Host "INFO: Executing Rscript $AnalysisScriptPath. This may take some time..."
    Write-Host "INFO: Output from the R script will be displayed below and also saved to $AnalysisOutputLog and $AnalysisErrorLog."
    
    try {
        $process = Start-Process Rscript -ArgumentList $AnalysisScriptPath -Wait -NoNewWindow -PassThru -RedirectStandardOutput $AnalysisOutputLog -RedirectStandardError $AnalysisErrorLog

        $analysisOutput = Get-Content $AnalysisOutputLog -Raw -ErrorAction SilentlyContinue
        $analysisError = Get-Content $AnalysisErrorLog -Raw -ErrorAction SilentlyContinue

        if ($analysisOutput) {
            Write-Host "--- R Script Standard Output (first 50 lines, see $AnalysisOutputLog for full) ---"
            Write-Host ($analysisOutput -split "`r`n" | Select-Object -First 50)
            Write-Host "--- End of R Script Standard Output Preview ---"
        }
        
        if ($process.ExitCode -ne 0) {
            Write-Error "ERROR: R analysis script failed with exit code $($process.ExitCode)."
            if ($analysisError) {
                Write-Error "--- R Script Standard Error (see $AnalysisErrorLog for full) ---"
                Write-Error $analysisError
                Write-Error "--- End of R Script Standard Error ---"
            }
            exit 1
        }
        
        if ($analysisError) {
            Write-Warning "--- R Script Standard Error (Warnings/Messages - see $AnalysisErrorLog for full) ---"
            Write-Warning ($analysisError -split "`r`n" | Select-Object -First 20)
            Write-Warning "--- End of R Script Standard Error Preview ---"
        }

        Write-Host "INFO: R analysis script execution complete. Check logs for details: $AnalysisOutputLog, $AnalysisErrorLog"
        Write-Host "INFO: Expected output files (e.g., event_study_plot.png, distribution.png) should be in the repository root: $RepoRoot"
    }
    catch {
        Write-Error "ERROR: An error occurred while running the R analysis script (PowerShell try/catch): $($_.Exception.Message)"
        Write-Error "Exception details: $($_.ToString())"
        if (Test-Path $AnalysisErrorLog) {
            $errContent = Get-Content $AnalysisErrorLog -Raw -ErrorAction SilentlyContinue
            if ($errContent) { Write-Error "R analysis error log content:`n$errContent" }
        }
        exit 1
    }
}

# --- Main Script Execution ---
Write-Host "INFO: Starting automated setup and execution for R analysis project."
Write-Host "INFO: Repository root assumed to be: $($RepoRoot.Path)"
Write-Host "INFO: Script logs will be generated in this directory for Renv setup and R analysis execution."

try {
    # Step 1: Install R
    Install-R

    # Step 2: Install Python
    Install-Python

    # Step 3: Check for renv.lock file
    Check-RenvLockFile

    # Step 4: Setup R Environment with renv
    Setup-REnvironment

    # Step 5: Check Data Requirements
    Check-DataFile

    # Step 6: Run the Analysis
    Run-RAnalysis

    Write-Host "INFO: All steps completed successfully."
}
catch {
    # This catch block handles errors from functions that use 'exit 1' or if $ErrorActionPreference = "Stop" causes termination.
    Write-Error "FATAL: Script execution failed. See previous error messages for details."
    if ($_.Exception) {
        Write-Error "Last PowerShell error: $($_.Exception.Message)"
        Write-Error "Full exception: $($_.ToString())"
    } elseif ($Error[0]) {
         Write-Error "Last PowerShell error record: $($Error[0].ToString())"
    }
    exit 1
}

Write-Host "INFO: Script finished."

