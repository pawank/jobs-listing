# PORTAL

## How to run?

`sbt`
`run`

## How to load prelim data?

1. Load Employer database from excel 
```
GET http://localhost:9000/document/excel/load

Query String:
filename /Users/pawan/git/github/pawank/sarkari-portal/data/EmployersExportable.xlsx

e.g http://localhost:9000/document/excel/load?filename=%2FUsers%2Fpawan%2Fgit%2Fgithub%2Fpawank%2Fsarkari-portal%2Fdata%2FEmployersExportable.xlsx
```

2. Load Education and skills (pre configured)
```
GET http://localhost:9000/data/load/education/skills
```

3. Load matching rules for 8th, 10th and 12th classes only
```
GET http://localhost:9000/data/load/matching/rules
```

## How to save to be crawling data source into the database
```
http://localhost:9000/backoffice/sites/crawler
```
Header: `Content-Type` = `application/json` 
and JSON payload as
```
{
	"name": "",
	"url": "http://www.targeturl.com/jobs/",
	"priority": 1,
	"status": ""
}
```

## How to load source URL for crawling data from file
```
http://localhost:9988/backoffice/testing?filename=load_crawling_data&url=local path to file
```
Provide `filename` as `load_crawling_data` and `url` as local file path.

## How to crawl data from sources, parse and prepare cache for ads
```
http://localhost:9988/backoffice/testing?filename=crawl&url=http%3A%2F%2Fwww.abc.com
```
Provide `filename` as `crawl` and `url` as target source URL. 
Provide `filename` as `crawl-all` then the engine will load all active SiteCrawler objects from the database and starts crawling one by one.

## How to crawl google search results matching with given pdf content
```
http://localhost:9988/backoffice/testing?filename=search
```
Provide `filename` as `crawl` and `url` as target source URL. 
Provide `filename` as `crawl-all` then the engine will load all active SiteCrawler objects from the database and starts crawling one by one.

## Load PDF from cache and create Ads (FullJob objects)
```
http://www.sarkarijoblisting.com/backoffice/data/load
Apply basic auth header
```

## Load crawled sites and convert them to FullJob / Ads
```
/backoffice/process/sites
Apply basic auth header
```

## Load Ads (FullJob objects) and convert them to Job types
```
http://www.sarkarijoblisting.com/backoffice/ads
Apply basic auth header
```

`sample` can be a count value to inform how many to process from starting of the list. 
Default value is all. 

## How to load SEO for pages?
```
POST http://localhost:9000/pages
HEADERS:
Content-Type application/json
Payload:
{
	"title": "Homepage",
	"description": "Welcome to home",
	"siteName": "home",
	"image": null,
	"pageType": "Home",
	"tags": [
		"Portal",
		"Best search"
	],
	"slug": "/home",
	"dates": null
}
```

## How to get content / refLink directly for a give Ad ID
```
GET http://localhost:9000/backoffice/ads/<ID>?fulljob=content
```
or
```
GET http://localhost:9000/backoffice/ads/<ID>?fulljob=refLink
```
Possible values for `fulljob` are `content`, `refLink`.


