// Import Montgomery County, MD shapefile.
var montgomeryCounty = ee.FeatureCollection('TIGER/2018/Counties')
  .filter(ee.Filter.eq('NAME', 'Montgomery'))
  .filter(ee.Filter.eq('STATEFP', '24')); // Maryland's state FIPS code is 24

// Define the date range.
var startYear = 2017;
var endYear = ee.Date(Date.now()).get('year');

// PRISM dataset (Daily Maximum Temperature)
var prism = ee.ImageCollection('OREGONSTATE/PRISM/AN81d')
  .select('tmax') // Use the maximum temperature band
  // .filterBounds(ca_zip_sample);

// Function to calculate total exceedance days at the county level in a year.
var calculateDays = function(year) {
  var startDate = ee.Date.fromYMD(year, 1, 1);
  var endDate = startDate.advance(1, 'year');

  // Convert Fahrenheit to Celsius: (°F - 32) * 5/9
  var threshold = ee.Number(80).subtract(32).multiply(5).divide(9); // 80°F = ~26.67°C

  // Filter the ImageCollection for the year
  var yearlyCollection = prism.filterDate(startDate, endDate)
    .map(function(image) {
      // Calculate the county-level mean temperature for the day
      var zipTemp = image.reduceRegion({
        reducer: ee.Reducer.mean(),
        geometry: montgomeryCounty.geometry(),
        scale: 4000,
        maxPixels: 1e9
      }).get('tmax');
      
      // Create a feature with a binary exceedance value (1 if > threshold)
      return ee.Feature(null, {
        'date': image.date().format('YYYY-MM-dd'),
        'exceedance': ee.Algorithms.If(ee.Number(zipTemp).gt(threshold), 1, 0)
      });
    });

  // Aggregate the number of exceedance days in the year
  var totalDays = yearlyCollection.aggregate_sum('exceedance');
  
  return ee.Feature(null, { 'year': year, 'daysAbove80F': totalDays });
};

// Map the function over each year.
var years = ee.List.sequence(startYear, endYear);
var yearlyCounts = ee.FeatureCollection(years.map(calculateDays));

// Print results for testing
print('Yearly counts:', yearlyCounts);

// Export the results to a CSV file.
Export.table.toDrive({
  collection: yearlyCounts,
  description: 'MontgomeryCounty_DaysAbove80F_CountyLevel',
  fileFormat: 'CSV'
});
