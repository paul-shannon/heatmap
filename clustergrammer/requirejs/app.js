require.config({
    paths: {
        'underscore'    :  'https://cdnjs.cloudflare.com/ajax/libs/underscore.js/1.8.3/underscore-min',
        'jquery'        :  'http://code.jquery.com/jquery-1.12.4.min',
        'clustergrammer':  'lib/js/clustergrammer'
        },
    shim: {underscore: {
             exports: '_'
             }}
    });

require(['underscore', 'jquery',  'clustergrammer'], function (_, $, clustergrammer) {

    window.cwg = clustergrammer;
    make_clust(cgStructure); // options: demo3x3, demoUSArrestsSmall (is 3x4), demoUSArrests (full data set), demoBioMatrix, demoBioMatrix2, demoMicroGlial, cgStructure CHANGE THIS TO CHANGE CLUSTERGRAMMER

    var about_string = 'Zoom, scroll, and click buttons to interact with the clustergram. <a href="http://amp.pharm.mssm.edu/clustergrammer/help"> <i class="fa fa-question-circle" aria-hidden="true"></i> </a>';
    function make_clust(network_data){
          var args = {
            root: '#cgDiv',
            'network_data': network_data,
            'about': "Example of clustered matrix from R. Compares 302 genes with 264 samples",
	      'row_label': "Genes",
	      'col_label': "Samples",
	      'tile_colors': ['#FF4500', '#307D7E'],
	      //'dendro_callback': 'console.log("dendro test")',
	      'super_label_scale': '3',
	      //'tile_tip_callback': 
            'sidebar_width':150,
            };
         var screen_width = window.innerWidth;
         var screen_height = window.innerHeight - 20;
         $("#cgDiv").width(screen_width);
         $("#cgDiv").height(screen_height);
         cgm = Clustergrammer(args);
         $("#cgDiv .wait_message").remove()
         console.log('loading clustergrammer')
      } // make_clust
    }); // require
