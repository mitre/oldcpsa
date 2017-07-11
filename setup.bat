@setlocal enableextensions
@cd /d "%~dp0"
runghc Setup.hs configure
runghc Setup.hs build

runghc Setup.hs install

pause
