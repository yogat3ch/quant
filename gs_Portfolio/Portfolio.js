var sheets = ['Personal'];
function col(column, lr, data) {
      var co = []
      for (var i = 0; i < lr; i++) {
        co.push(data[i][column])
      }
      return(co)
   }

function getLimits(x) {
   return(x > 1)
  };

function logicSub(ind, arr) {
    var index_arr = []
    for(var i = 0; i < arr.length; i++){
      if(ind[i]){
    index_arr.push(arr[i]);
      }
    }
    return(index_arr)
  }

function logicInd(ind) {
    var index_arr = []
    for(var i = 0; i < ind.length; i++){
      if(ind[i]){
    index_arr.push(i);
      }
    }
    return(index_arr)
  } // Get the indexes of the T values in the boolean

function ArrIndSub(arr,indexArr){
  for(var i = 0; i < indexArr.length; i++)
  var resultArr = []
  resultArr.push(arr[indexArr[i]]);
  return(resultArr)
} // Subset an array with an array of indices

function isValidDate(d) {
  if ( Object.prototype.toString.call(d) !== "[object Date]" )
    return false;
  return !isNaN(d.getTime());
} // Check if a date is an actual date

function Update(){
  
  function DailyUpdate(sheet) {
    // Global Vars
    var sheet = SpreadsheetApp.getActiveSpreadsheet();
    var sh = sheet.getSheetByName(sheets[0]);
    var lr = sh.getLastRow()
    var data = sh.getRange(1, 1, lr, sh.getLastColumn()).getValues();
    // End Global vars
    // Limit Notification
  var ntfy = col(data[0].indexOf('LimNotify'),lr,data); //Get the column of data labeled LimNotify
  var arr_n = ntfy.map(Number); // Make sure they are numbers
  arr_n = arr_n.map(getLimits) // Make an array (arr_n) of boolean indicating where values are in LimNotify column
  var limits = logicSub(arr_n,data); // Return only those rows where there are values
    for(i = 0; i < limits.length; i++){ // For each one extract the following values into legible variables, create msg and send it
      var price = limits[i][data[0].indexOf('Price')];
      var limit = limits[i][data[0].indexOf('LimNotify')];
      var sym = limits[i][data[0].indexOf('Symbol')];
      var not = limits[i][data[0].indexOf('LimNotified')];
      
      if(isValidDate(not)){ //Check date validity
        if(String(not).length > 0){
          var today = new Date();
          today = today.toISOString().split('T')[0];
          var notified = Utilities.formatDate(not, SpreadsheetApp.getActiveSpreadsheet().getSpreadsheetTimeZone(), "yyyy-MM-dd");
          var previous = notified != today; // Boolean if already notified today or not
        }
      }
      
      
      if(price > limit && previous){ // If price exceeds limit and not notified yet, send a text message and update the notified values
        var msg = sym + ': ' + price + ' > ' + limit
        MailApp.sendEmail("7818797492@vtext.com", '#GSLim: ' + msg,"", {name: '#GSAlert'})
        var d = sh.getRange(logicInd(arr_n)[i] + 1, data[0].indexOf('LimNotified') + 1).setValue(today)
      }
    }
    
   // Stop Loss notifications
  var stls = col(data[0].indexOf('StLoss'),lr,data); //Get the column of data labeled StLoss
  arr_n = stls.map(Number);  //Make numbers of stop loss values
  arr_n = arr_n.map(getLimits) //Boolean array
  var t = arr_n.filter(Boolean).length // # true
  var ind = logicInd(arr_n)
  // Create an array of the relevant data
  var init_compare = [col(data[0].indexOf('Symbol'),lr,data),col(data[0].indexOf('Initial'),lr,data),stls,col(data[0].indexOf('Price'),lr,data),col(data[0].indexOf('SLNotified'),lr,data),col(data[0].indexOf('Qty'),lr,data)] 
    msg = [] // Pre create a message array
    for (i = 0; i < t; i++) {
      if (init_compare[5][ind[i]] > 0 && init_compare[3][ind[i]] < init_compare[2][ind[i]] | init_compare[3][ind[i]] < init_compare[1][ind[i]] * 1.1) {
      var p = (init_compare[3][ind[i]] - init_compare[1][ind[i]]) / init_compare[1][ind[i]];
      var sym = init_compare[0][ind[i]]
      msg[i] = sym + ': P = ' + init_compare[3][ind[i]].toFixed(2) + ', SL = ' + init_compare[2][ind[i]].toFixed(2) + ', I = ' + init_compare[1][ind[i]] + ', %G/L = ' + p.toFixed(2)
      // Check to see if it is already sent
      var not = data[ind[i]][data[0].indexOf('SLNotified')]
      var notified = isValidDate(not)
      if(isValidDate(not)){
        if(String(not).length > 0){
        var notified = Utilities.formatDate(not, SpreadsheetApp.getActiveSpreadsheet().getSpreadsheetTimeZone(), "yyyy-MM-dd");
        }
      } // date validity
        var today = new Date();
        today = today.toISOString().split('T')[0];
        if (notified != today | notified === undefined) var d = sh.getRange(ind[i] + 1, data[0].indexOf('SLNotified') + 1).setValue(today)
     } //init compare
    }
  msg = logicSub(msg.map(Boolean),msg)
  
  
    if (notified != today) {
      for (i = 0; i < msg.length; i++){
        MailApp.sendEmail("7818797492@vtext.com", '#GSSL', msg[i], {name: '#GSAlert'})
      }
    }
  // Send Updates on all values
  var mets = [col(data[0].indexOf("Symbol"),lr,data),col(data[0].indexOf("ß2Q"),lr,data),col(data[0].indexOf("ß1M"),lr,data),
col(data[0].indexOf("µMo-µ2Q/σ2Q"),lr,data),col(data[0].indexOf("µ2w-µ1Q/σ1Q"),lr,data)]
  arr_n = mets[1].map(Number);
  var arr_b = arr_n.map(Boolean);
  t = arr_b.filter(Boolean).length
  ind = logicInd(arr_b)
  msg = [] // Pre create a message array
    for (i = 0; i < t; i++) {
      msg[i] = mets[0][ind[i]] + ': 2Q = ' + mets[1][ind[i]].toFixed(2) + ', 1M = ' + mets[2][ind[i]].toFixed(2) + ', Mo-2Q/2Q = ' + mets[3][ind[i]].toFixed(2) + ', 2W-1Q/1Q = ' + mets[4][ind[i]].toFixed(2)
      // Check to see if it is already sent
      var not = data[ind[i]][data[0].indexOf('Updated')]
      var notified = isValidDate(not)
      if(isValidDate(not)){
        if(String(not).length > 0){
        var notified = Utilities.formatDate(not, SpreadsheetApp.getActiveSpreadsheet().getSpreadsheetTimeZone(), "yyyy-MM-dd");
        }
      } // date validity
        var today = new Date();
        today = today.toISOString().split('T')[0];
        if (notified != today | notified === undefined) var d = sh.getRange(ind[i] + 1, data[0].indexOf('Updated') + 1).setValue(today)
     } //init compare
    
  
  msg = logicSub(msg.map(Boolean),msg)
  
  
    if (notified != today) {
      for (i = 0; i < msg.length; i++){
        MailApp.sendEmail("7818797492@vtext.com", '#GSUpdate', msg[i], {name: '#GSAlert'})
      }
      
    }
  }
sheets.forEach(DailyUpdate)
} // Send an update each day

