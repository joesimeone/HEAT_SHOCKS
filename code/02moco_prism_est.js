// Import Montgomery County, MD shapefile.
var montgomeryCounty = ee.FeatureCollection('TIGER/2018/Counties')
  .filter(ee.Filter.eq('NAME', 'Montgomery'))
  .filter(ee.Filter.eq('STATEFP', '24')); // Maryland's state FIPS code is 24
  

var uniqueID = 'GEOID';
var featColl = montgomeryCounty.select(uniqueID);

// Print the filtered feature collection to verify it's not empty
print('Filtered Feature Collection Size:', featColl.size());
print('Sample Feature from Filtered Collection:', featColl.limit(1));
Map.addLayer(featColl);

  
  // Specify years of interest.
var startYear = 2017;
var endYear = 2024;

var imageCollectionName = 'OREGONSTATE/PRISM/AN81d';
var bandsWanted = ['ppt', 'tmean', 'tmax', 'tmin', 'tdmean'];
var scale = 4000;

// Export info
var exportFolder = 'GEE_PRISM_MONTCO_ZIP';
var filenameBase = 'PRISM_MONTCO_' + scale + 'm_';

// Initiate a loop, in which the variable i takes on values of each year.
for (var i = startYear; i <= endYear; i++) { // for each year....

  // Load climate collection for that year.
  var startDate = i + '-01-01';
  var endYear_adj = i + 1;
  var endDate = endYear_adj + '-01-01';

  var imageCollection = ee.ImageCollection(imageCollectionName)
      .select(bandsWanted)
      .filterBounds(featColl)
      .filterDate(startDate, endDate);

  // Print the image collection to verify it's being loaded correctly
  print('Image Collection for year ' + i, imageCollection);

  // Get values at feature collection.
  var sampledFeatures = imageCollection.map(function(image) {
    return image.reduceRegions({
      collection: featColl,
      reducer: ee.Reducer.mean(),
      tileScale: 1,
      scale: scale
    }).filter(ee.Filter.notNull(bandsWanted)) // remove rows without data
      .map(function(f) { // add date property
        var time_start = image.get('system:time_start');
        var dte = ee.Date(time_start).format('YYYYMMdd');
        return f.set('date_ymd', dte);
      });
  }).flatten();

  // Print sampled features to verify they contain data
  print('Sampled Features for year ' + i, sampledFeatures);

  // Prepare export: specify properties and filename.
  var columnsWanted = [uniqueID].concat(['date_ymd'], bandsWanted);
  var filename = filenameBase + i;

  Export.table.toDrive({
    collection: sampledFeatures,
    description: filename,
    folder: exportFolder,
    fileFormat: 'CSV',
    selectors: columnsWanted
  });

}