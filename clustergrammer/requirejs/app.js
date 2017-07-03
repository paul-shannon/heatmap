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

require(['underscore', 'jquery', 'clustergrammer'], function (_, $, clustergrammer) {
    window.cwg = clustergrammer;
    network_data =
        {"views":
         [{"N_row_sum": "all",
           "dist": "cos",
           "nodes":
             {"col_nodes": [{"ini": 3, "rank": 2, "name": "s01_120405", "clust": 1,
                             "group": [1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0], "rankvar": 2},
                            {"ini": 2, "rank": 1, "name": "s02_120405", "clust": 0,
                             "group": [3.0, 3.0, 2.0, 2.0, 2.0, 2.0, 2.0, 1.0, 1.0, 1.0, 1.0], "rankvar": 0},
                            {"ini": 1, "rank": 0, "name": "s03_120405", "clust": 2,
                             "group": [2.0, 2.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0], "rankvar": 1}],
              "row_nodes": [{"ini": 2, "rank": 0, "name": "HLTF", "clust": 0,
                             "group": [1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0], "rankvar": 0},
                            {"ini": 1, "rank": 1, "name": "POU2F1", "clust": 1,
                             "group": [2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 1.0], "rankvar": 1}]}},
            {"dist": "cos", "N_row_var": "all", "nodes":
           {"col_nodes": [{"ini": 3, "rank": 2, "name": "s01_120405", "clust": 1,
                           "group": [1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0], "rankvar": 2},
                          {"ini": 2, "rank": 1, "name": "s02_120405", "clust": 0,
                           "group": [3.0, 3.0, 2.0, 2.0, 2.0, 2.0, 2.0, 1.0, 1.0, 1.0, 1.0], "rankvar": 0},
                          {"ini": 1, "rank": 0, "name": "s03_120405", "clust": 2,
                           "group": [2.0, 2.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0], "rankvar": 1}],
            "row_nodes": [{"ini": 2, "rank": 0, "name": "HLTF", "clust": 0,
                           "group": [1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0], "rankvar": 0},
                          {"ini": 1, "rank": 1, "name": "POU2F1", "clust": 1,
                           "group": [2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 1.0], "rankvar": 1}]}}],
         "col_nodes": [{"ini": 3, "rank": 2, "name": "s01_120405", "clust": 1,
                        "group": [1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0], "rankvar": 2},
                       {"ini": 2, "rank": 1, "name": "s02_120405", "clust": 0,
                        "group": [3.0, 3.0, 2.0, 2.0, 2.0, 2.0, 2.0, 1.0, 1.0, 1.0, 1.0], "rankvar": 0},
                       {"ini": 1, "rank": 0, "name": "s03_120405", "clust": 2,
                        "group": [2.0, 2.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0], "rankvar": 1}],
         "links": [{"target": 0, "value": 5.591991, "source": 0},
                   {"target": 1, "value": 11.939007, "source": 0},
                   {"target": 2, "value": 7.738552, "source": 0},
                   {"target": 0, "value": 31.060965999999997, "source": 1},
                   {"target": 1, "value": 18.00348, "source": 1},
                   {"target": 2, "value": 21.577569, "source": 1}],
         "row_nodes": [{"ini": 2, "rank": 0, "name": "HLTF", "clust": 0,
                        "group": [1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0], "rankvar": 0},
                       {"ini": 1, "rank": 1, "name": "POU2F1", "clust": 1,
                        "group": [2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 1.0], "rankvar": 1}]};


    var tinyData = {"row_nodes": [{"name": "Row: R1", "ini": 3, "clust": 1, "rank": 0,
          "rankvar": 0, "group": [1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
          1.0, 1.0]}, {"name": "Row: R2", "ini": 2, "clust": 0, "rank": 2,
          "rankvar": 2, "group": [3.0, 3.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 1.0,
          1.0, 1.0]}, {"name": "Row: R3", "ini": 1, "clust": 2, "rank": 1,
          "rankvar": 1, "group": [2.0, 2.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
          1.0, 1.0]}], "col_nodes": [{"name": "Col: C1", "ini": 3, "clust": 1,
          "rank": 1, "rankvar": 1, "group": [1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
          1.0, 1.0, 1.0, 1.0]}, {"name": "Col: C2", "ini": 2, "clust": 0,
          "rank": 0, "rankvar": 0, "group": [3.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0,
          2.0, 2.0, 2.0, 1.0]}, {"name": "Col: C3", "ini": 1, "clust": 2,
          "rank": 2, "rankvar": 2, "group": [2.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
          1.0, 1.0, 1.0, 1.0]}], "links": [], "mat": [[0.0, -0.1, 1.0], [3.0,
          0.0, 8.0], [0.2, 0.1, 2.5]], "cat_colors": {"row": {}, "col": {}},
          "views": [{"N_row_sum": "all", "dist": "cos", "nodes": {"row_nodes":
          [{"name": "Row: R1", "ini": 3, "clust": 1, "rank": 0, "rankvar": 0,
          "group": [1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]},
          {"name": "Row: R2", "ini": 2, "clust": 0, "rank": 2, "rankvar": 2,
          "group": [3.0, 3.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 1.0, 1.0, 1.0]},
          {"name": "Row: R3", "ini": 1, "clust": 2, "rank": 1, "rankvar": 1,
          "group": [2.0, 2.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]}],
          "col_nodes": [{"name": "Col: C1", "ini": 3, "clust": 1, "rank": 1,
          "rankvar": 1, "group": [1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
          1.0, 1.0]}, {"name": "Col: C2", "ini": 2, "clust": 0, "rank": 0,
          "rankvar": 0, "group": [3.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0,
          2.0, 1.0]}, {"name": "Col: C3", "ini": 1, "clust": 2, "rank": 2,
          "rankvar": 2, "group": [2.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
          1.0, 1.0]}]}}, {"N_row_var": "all", "dist": "cos", "nodes":
          {"row_nodes": [{"name": "Row: R1", "ini": 3, "clust": 1, "rank": 0,
          "rankvar": 0, "group": [1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
          1.0, 1.0]}, {"name": "Row: R2", "ini": 2, "clust": 0, "rank": 2,
          "rankvar": 2, "group": [3.0, 3.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 1.0,
          1.0, 1.0]}, {"name": "Row: R3", "ini": 1, "clust": 2, "rank": 1,
          "rankvar": 1, "group": [2.0, 2.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
          1.0, 1.0]}], "col_nodes": [{"name": "Col: C1", "ini": 3, "clust": 1,
          "rank": 1, "rankvar": 1, "group": [1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
          1.0, 1.0, 1.0, 1.0]}, {"name": "Col: C2", "ini": 2, "clust": 0,
          "rank": 0, "rankvar": 0, "group": [3.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0,
          2.0, 2.0, 2.0, 1.0]}, {"name": "Col: C3", "ini": 1, "clust": 2,
          "rank": 2, "rankvar": 2, "group": [2.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
          1.0, 1.0, 1.0, 1.0]}]}}]}

    make_clust(tinyData);

    var about_string = 'Zoom, scroll, and click buttons to interact with the clustergram. <a href="http://amp.pharm.mssm.edu/clustergrammer/help"> <i class="fa fa-question-circle" aria-hidden="true"></i> </a>';
    function make_clust(network_data){
          var args = {
            root: '#cgDiv',
            'network_data': network_data,
            'about': about_string,
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
