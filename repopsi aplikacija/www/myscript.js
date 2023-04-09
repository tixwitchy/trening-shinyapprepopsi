function loadScript(url)
{    
    var head = document.getElementsByTagName('head')[0];
    var script = document.createElement('script');
    script.type = 'text/javascript';
    script.src = url;
    head.appendChild(script);
}


loadScript('https://cdn.datatables.net/1.13.3/js/jquery.dataTables.min.js');


function filterGlobal() {
    $('#DataTables_Table_0')
        .DataTable()
        .search($('#global_filter').val())
        .draw();
}
 

 
$(document).ready(function () {
    $('#DataTables_Table_0').DataTable();
 
    $('input.global_filter').on('keyup click', function () {
        filterGlobal();
    });

});