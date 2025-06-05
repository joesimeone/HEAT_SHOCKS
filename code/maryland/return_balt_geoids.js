// Use the standard US counties dataset
var mdCounties = ee.FeatureCollection('TIGER/2018/Counties');

// Filter to Maryland counties
var mdStandardCounties = mdCounties.filter(ee.Filter.eq('STATEFP', '24'));

// Add Maryland counties to the map
Map.centerObject(mdStandardCounties, 8);
Map.addLayer(mdStandardCounties, {color: 'blue'}, 'Maryland Counties');

// Filter for Baltimore City and County specifically
var baltimoreCity = mdStandardCounties.filter(ee.Filter.eq('NAME', 'Baltimore city'));
var baltimoreCounty = mdStandardCounties.filter(ee.Filter.eq('NAME', 'Baltimore'));

// Combine them
var baltimoreBoth = baltimoreCity.merge(baltimoreCounty);

// Highlight on map
Map.addLayer(baltimoreCity, {color: 'red'}, 'Baltimore City');
Map.addLayer(baltimoreCounty, {color: 'yellow'}, 'Baltimore County');

// Print only Baltimore City and County information
var counties = baltimoreBoth.toList(baltimoreBoth.size());
counties.evaluate(function(countyList) {
  for (var i = 0; i < countyList.length; i++) {
    var county = countyList[i];
    print('County:', county.properties.NAME, 
          'GEOID:', county.properties.GEOID,
          'system:index:', county.properties['system:index'],
          'Feature ID:', county.id);
  }
});

// Alternative approach - print as a feature collection
print('Baltimore Areas with IDs:', 
  baltimoreBoth.select(['NAME', 'GEOID', 'COUNTYFP', 'system:index']));