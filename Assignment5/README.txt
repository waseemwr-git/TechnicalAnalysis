BDA400 Assignment 5 - Technical Analysis using R, Development Phase
Student: Waseem Rahmathulla Malayilthodi

Repository placement:
TechnicalAnalysis/
└── Assignment5/
    ├── sma.R
    ├── ema.R
    ├── macd.R
    ├── stdev.R
    ├── linreg.R
    ├── rsi.R
    ├── stoch_rsi.R
    ├── crossover.R
    ├── crossunder.R
    ├── run_tests.R
    └── WaseemRahmathullaMalayilthodi_BDA400_A05.docx

How to run:
1. Put all files in the same Assignment5 folder.
2. Open RStudio.
3. Set the working directory to the Assignment5 folder.
4. Open and run run_tests.R.
5. The test runner sources all nine required indicator files in dependency order.
6. No external R packages are required.

Important:
- Keep the function names exactly as supplied.
- macd() depends on ema().
- stoch_rsi() depends on rsi() and sma().
- Repository: https://github.com/waseemwr-git/TechnicalAnalysis
