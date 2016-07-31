function populateTable(list) {
    var header = "<tr><th>Service</th><th>Quantity</th><th>Middle something time</th></tr>";
    alert(JSON.stringify(list));
    var content = "";
    var record;
    for(var i = 0; i < list.length; i++) {
        alert(JSON.stringify(list[i]));
        content += "<tr><th>" + list[i].calltblProgram +
                   "</th><th>" + list[i].calltblCity + 
                   "</th><th>" + list[i].calltblCalltype + 
                   "</th></tr>"; 
    }
    document.getElementById("table").innerHTML = header + content;
};
function funcsubmit() {
    var xhttp = new XMLHttpRequest();
    xhttp.onreadystatechange = function() {
          document.getElementById("table").innerHTML;
          populateTable(JSON.parse(xhttp.responseText));
    };
    var program = document.getElementById("program").value;
    var programString = program?"&&program="+program:"";
    var city = document.getElementById("city").value;
    var cityString = city?"&&city="+city:"";
    var partner = document.getElementById("partner").value;
    var partnerString = partner?"&&partner="+partner:"";
    var fromDate = document.getElementById("fromDate").value;
    var fromDateString = fromDate?"&&fromDate="+fromDate:"";
    var toDate = document.getElementById("toDate").value;
    var toDateString = toDate?"&&toDate="+toDate:"";
    var get = "submit?true=true" + programString + cityString + partnerString + fromDateString + toDateString;
    xhttp.open("GET", get, true);
    xhttp.send();
};
function populateCombobox(box, values) {
    var options = "";
    for(var i = 0; i < values.length; i++) {
        options += "<option value=\"#\">#</option>".replace(/#/g, values[i]);
    }
    document.getElementById(box).innerHTML = options;
};
function initialiseComboboxes() {
    var xhttp = new XMLHttpRequest();
    xhttp.onreadystatechange = function() {
          var resp = JSON.parse(xhttp.responseText);
          populateCombobox("program", resp.programs);
          populateCombobox("city", resp.cities);
          populateCombobox("partner", resp.partners);
    };
    var program = document.getElementById("program").value;
    var post = "options";
    xhttp.open("post", post, false);
    xhttp.send();
};
initialiseComboboxes();
