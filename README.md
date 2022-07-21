# Forest diversity dataset
This folder contains scripts related to the synthesis forest diversity datasets
The most up-to-date versions of the dataset are:
- 31206 Assembled RAW diversity from forest EPs (2007-2020) for multidiversity synthesis - January 2022
- 31207 Assembled species information from forest EPs (2007-2020) for multidiversity synthesis - January 2022
Please check our [manual](https://raw.githubusercontent.com/biodiversity-exploratories-synthesis/Synthesis_dataset_manual/main/Synthesis%20datasets%20%20How%20to%20use.md) before using the datasets.

-------------------------------------------------------------------------------------------------------------------------- 
**-->From the following Bexis datasets:**
- 24607	Assembled RAW diversity from forest EPs (2007-2015) for multidiversity synthesis
- 24608	Assembled species information from forest EPs (2007-2015) for multidiversity synthesis

**--> To the following Bexis datasets:**
- 31206 Assembled RAW diversity from forest EPs (2007-2020) for multidiversity synthesis - January 2022
- 31207 Assembled species information from forest EPs (2007-2020) for multidiversity synthesis - January 2022

**Scripts**
  - 200220_UpdateGRLdataset.R -- Final script

--------------------------------------------------------------------------------------------------------------------------  
**-->From single datasets**
**--> To the following Bexis datasets:**
- 24607	Assembled RAW diversity from forest EPs (2007-2015) for multidiversity synthesis
- 24608	Assembled species information from forest EPs (2007-2015) for multidiversity synthesis

**Scripts**
  - 190215_DataCleaningFOR.R -- Read and prepare single datasets
  - 190215_DataAssemblingRawAbund.R -- Assemble all previously prepared datasets
  - 190222_Upload_Bexis_FOR.R -- Prepare dataset for upload to Bexis
