[![UpdateDB](https://github.com/the-Hull/weekly_oil_bulletin/actions/workflows/update_db.yaml/badge.svg)](https://github.com/the-Hull/weekly_oil_bulletin/actions/workflows/update_db.yaml)

# Weekly Oil Bulletin Database

This repository contains the collated price data of petroleum products across EU countries and the UK, which is published in the Weekly Oil Bulletin through the European Commission.
More information on the bulletin can be found here: [https://energy.ec.europa.eu/data-and-analysis/weekly-oil-bulletin_en](https://energy.ec.europa.eu/data-and-analysis/weekly-oil-bulletin_en)

## Database

The data is stored in [data/db](data/db), with a two identical files - one in binary (`.rds`) and in text (`.csv`) formats.
Both files have identical contents.

## Method

The bulletin is released as a [PDF file](https://ec.europa.eu/energy/observatory/reports/List-of-WOB.pdf) containing a list of prices (links to `xls` or `xlsx` files).
Based on the PDF and publishing dates, download links for individual bulletins are created, downloaded and re-formatted for storing in the database.
A central log file tracks the status of downloads and data base creation, and is updated on each run.

## Latest update:

Date: 23 April, 2024 at 16:06

## First look:

![German Oil Bulletin Prices](https://github.com/the-hull/weekly_oil_bulletin/blob/main/fig/wob_germany.png?raw=true)


