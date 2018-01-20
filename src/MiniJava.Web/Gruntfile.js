module.exports = function(grunt) {
    grunt.initConfig({
        clean: ["wwwroot/sources", "wwwroot/lib"],
        uglify: {
            all: {
                src: ["wwwroot/js/site.js"],
                dest: "wwwroot/js/site.min.js"
            }
        },
        cssmin: { target: { files: { "wwwroot/css/site.min.css": ["wwwroot/css/site.css"] } } },
        copy: {
            main: {
                files: [
                    {
                        cwd: "../../resources",
                        src: "**/*",
                        dest: "wwwroot/sources",
                        expand: true
                    },
                    {
                        cwd: "node_modules/ace-builds",
                        src: ["src-noconflict/**", "src-min-noconflict/**"],
                        dest: "wwwroot/lib/ace-builds",
                        expand: true
                    },
                    {
                        cwd: "node_modules/bootstrap/dist",
                        src: "**/*",
                        dest: "wwwroot/lib/bootstrap",
                        expand: true
                    },
                    {
                        cwd: "node_modules/bootstrap-treeview/dist",
                        src: "**/*",
                        dest: "wwwroot/lib/bootstrap-treeview",
                        expand: true
                    },
                    {
                        cwd: "node_modules/font-awesome",
                        src: ["css/**", "fonts/**"],
                        dest: "wwwroot/lib/font-awesome",
                        expand: true
                    },
                    {
                        cwd: "node_modules/jquery/dist",
                        src: "**/*",
                        dest: "wwwroot/lib/jquery",
                        expand: true
                    },
                    {
                        cwd: "node_modules/popper.js/dist",
                        src: "**/*",
                        dest: "wwwroot/lib/popper.js",
                        expand: true
                    }
                ]
            }
        }
    });

    grunt.loadNpmTasks("grunt-contrib-clean");
    grunt.loadNpmTasks("grunt-contrib-uglify-es");
    grunt.loadNpmTasks("grunt-contrib-cssmin");
    grunt.loadNpmTasks("grunt-contrib-copy");
    grunt.registerTask("all", ["clean", "uglify", "cssmin", "copy"]);
};