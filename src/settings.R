

# File paths - define the paths to the data files
SHOOTINGS        <- "data/shootings.csv"        # https://data.cityofnewyork.us/Public-Safety/NYPD-Shooting-Incident-Data-Year-To-Date-/5ucz-vwe8/about_data
ARRESTS          <- "data/arrests.csv"          # https://data.cityofnewyork.us/Public-Safety/NYPD-Arrest-Data-Year-to-Date-/uip8-fykc/about_data
NYC_MONTH_CRIMES <- "data/nyc_month_crimes.csv" # Output file

# Colum names - define the columns to be used in the analysis
SHOOTINGS_COLS <- c(
    "INCIDENT_KEY",
    "OCCUR_DATE",
    "OCCUR_TIME",
    "BORO"
)

ARRESTS_COLS <- c(
    "ARREST_KEY",
    "ARREST_DATE",
    "ARREST_BORO",
    "PD_DESC"
)

# Name mappings

borough_name_map <- c(
    "S" = "STATEN ISLAND",
    "K" = "BROOKLYN",
    "M" = "MANHATTAN",
    "B" = "BRONX",
    "Q" = "QUEENS"
)

borough_name_map2 <- c(
    "STATEN ISLAND" = "StatenIsland",
    "BROOKLYN"      = "Brooklyn",
    "MANHATTAN"     = "Manhattan",
    "BRONX"         = "Bronx",
    "QUEENS"        = "Queens",
    "NEW YORK"      = "NewYork"
)

CRIMES <- list(
    Assaults = c( # Aggressione
        "ASSAULT 3",
        "ASSAULT 2,1,UNCLASSIFIED", 
        "ASSAULT 2,1,PEACE OFFICER",
        "ASSAULT POLICE/PEACE OFFICER"
    ),
    Marijuana = c(  # Possesso e vendita di marijuana
        "MARIJUANA, POSSESSION 4 & 5",
        "MARIJUANA, SALE 4 & 5",
        "MARIJUANA, POSSESSION 1, 2 & 3",
        "MARIJUANA, POSSESSION",
        "MARIJUANA, SALE 1, 2 & 3",
        "UNLAWFUL SALE SYNTHETIC MARIJUANA"
    ),
    Theft = c( # Furto
        "THEFT OF SERVICES, UNCLASSIFIED",
        "THEFT OF SERVICES, UNCLASSIFIE",
        "THEFT,RELATED OFFENSES,UNCLASSIFIED",
        "THEFT OF SERVICES- CABLE TV SERVICE",
        "THEFT,RELATED OFFENSES,UNCLASS"
    ),
    Firearm = c( # Arma da fuoco
        "C   1 & 2",
        "CRIMINAL DISPOSAL FIREARM 1 &",
        "CRIMINAL DISPOSAL FIREARM 1",
        "LICENSING FIREARMS",
        "FIREARMS LICENSING LAWS"
    ),
    SubstancePossession = c(  # Possesso/vendita di sostanza controllata
        "CONTROLLED SUBSTANCE, POSSESSION 7",
        "CONTROLLED SUBSTANCE,INTENT TO SELL 3",
        "CONTROLLED SUBSTANCE,SALE 3",
        "CONTROLLED SUBSTANCE, POSSESSI",
        "CONTROLLED SUBSTANCE, INTENT TO SELL 5",
        "CONTROLLED SUBSTANCE,INTENT TO",
        "CONTROLLED SUBSTANCE, POSSESSION 5",
        "CONTROLLED SUBSTANCE,POSSESS. 1",
        "CONTROLLED SUBSTANCE, POSSESSION 4",
        "CONTROLLED SUBSTANCE,POSSESS. 2",
        "CONTROLLED SUBSTANCE, SALE 5",
        "CONTROLLED SUBSTANCE,POSSESS.",
        "CONTROLLED SUBSTANCE, INTENT T",
        "CONTROLLED SUBSTANCE,SALE 1",
        "CONTROLLED SUBSTANCE,POSSESS. 3",
        "CONTROLLED SUBSTANCE,SALE 2",
        "CONTROLLED SUBSTANCE, SALE 4",
        "CONTROLLED SUBSTANCE,POSSESS. OF PROCURSERS"
    ),
    Robbery = c( # Rapina
        "ROBBERY,UNCLASSIFIED,OPEN AREAS",
        "ROBBERY,OPEN AREA UNCLASSIFIED",
        "ROBBERY,UNCLASSIFIED,OPEN AREA",
        "ROBBERY,CARJACKING OF MV OTHER THAN TRUCK",
        "ROBBERY,CAR JACKING",
        "ROBBERY,GAS STATION"
    ),
    Weapons = c( # Armi
        "WEAPONS, POSSESSION, ETC",
        "WEAPONS POSSESSION 3",
        "WEAPONS POSSESSION 1 & 2",
        "WEAPONS,MFR,TRANSPORT,ETC.",
        "WEAPONS,PROHIBITED USE",
        "WEAPONS,DISPOSITION OF",
        "WEAPONS DISPOSITION OF",
        "WEAPONS,PROHIBITED USE IMITATION PISTOL",
        "WEAPONS,PROHIBITED USE IMITATI"
    ),
    DrugParaphernalia = c(  # Legami per droga
        "DRUG PARAPHERNALIA,   POSSESSES OR SELLS 2",
        "DRUG PARAPHERNALIA,   POSSESSE",
        "DRUG PARAPHERNALIA,   POSSESSES OR SELLS 1"
    ),
    Larceny= c(  # Furto
        "LARCENY,PETIT FROM OPEN AREAS,UNCLASSIFIED",
        "LARCENY,PETIT FROM OPEN AREAS,",
        "LARCENY,GRAND FROM OPEN AREAS,UNCLASSIFIED",
        "LARCENY,GRAND FROM OPEN AREAS, UNATTENDED",
        "LARCENY,GRAND OF AUTO",
        "LARCENY,GRAND FROM PERSON,UNCLASSIFIED",
        "LARCENY,GRAND FROM PERSON,UNCL",
        "LARCENY,GRAND FROM OPEN AREAS,",
        "LARCENY,GRAND BY EXTORTION",
        "AGGRAVATED GRAND LARCENY OF ATM",
        "LARCENY,GRAND BY CREDIT CARD USE",
        "LARCENY,PETIT BY ACQUIRING LOS",
        "LARCENY,GRAND BY ACQUIRING LOS",
        "LARCENY,GRAND FROM BUILDING,UNCLASSIFIED",
        "LARCENY,GRAND BY THEFT OF CREDIT CARD",
        "LARCENY,GRAND FROM BUILDING (NON-RESIDENCE) UNATTENDED"
    ),
    Mischief = c( # Frode
        "MISCHIEF,CRIMINAL     UNCLASSIFIED 4TH DEG",
        "MISCHIEF,CRIMINAL,    UNCL 2ND",
        "MISCHIEF,CRIMINAL,    UNCL 2ND DEG 3RD DEG",
        "CRIMINAL MISCHIEF,UNCLASSIFIED 4",
        "CRIMINAL MISCHIEF 4TH, GRAFFITI",
        "CRIMINAL MISCHIEF 4TH, GRAFFIT",
        "MISCHIEF, CRIMINAL 4, OF MOTOR VEHICLE",
        "MISCHIEF, CRIMINAL 3 & 2, OF MOTOR VEHICLE",
        "MISCHIEF 1,CRIMINAL,EXPLOSIVE",
        "MISCHIEF, CRIMINAL 4, BY FIRE",
        "MISCHIEF, CRIMINAL 3 & 2, OF M",
        "MISCHIEF, CRIMINAL 4, OF MOTOR",
        "MISCHIEF,CRIMINAL     UNCLASSI"
    ),
    Trepass = c(  # Trasgressione
        "TRESPASS 3, CRIMINAL",
        "TRESPASS 2, CRIMINAL",
        "TRESPASS 4,CRIMINAL",
        "TRESPASS 1,CRIMINAL",
        "TRESPASS 4,CRIMINAL SUB 2"
    ),
    Burglary = c(  # Furto con scasso
        "BURGLARY,UNCLASSIFIED,UNKNOWN TIME",
        "BURGLARY,UNCLASSIFIED,UNKNOWN",
        "BURGLARY,RESIDENCE,NIGHT",
        "BURGLARY,COMMERCIAL,DAY"
    ),
    BRIBERY = c(  # Corruzione
        "BRIBERY,PUBLIC ADMINISTRATION",
        "BRIBERY,COMMERCIAL",
        "BRIBERY,FRAUD"
    ),
    Murder = c(  # Omicidio
        "MURDER,UNCLASSIFIED"
    ),
    Forgery = c(  # Falsificazione
        "FORGERY,ETC.,UNCLASSIFIED-FELONY",
        "FORGERY,ETC.-MISD.",
        "FORGERY,ETC.,UNCLASSIFIED-FELO",
        "FORGERY,M.V. REGISTRATION",
        "FORGERY-ILLEGAL POSSESSION,VEH",
        "FORGERY-ILLEGAL POSSESSION,VEHICLE IDENT. NU",
        "FORGERY,PRESCRIPTION"
    )
)
