const home = (function() {
    "use strict";

    const init = function() {
        const hiddenSource = document.getElementById("hidden-source");
        const editor = ace.edit("editor");
        const changeSource = function(source) {
            hiddenSource.value = source;
            editor.setValue(source, 0);
            editor.gotoLine(0);
        };

        const changeSubmit = function(error) {
            document.getElementById("submit").disabled = error !== null;
        };

        const timer = document.getElementById("timer");
        const changeTimer = function(t) {
            timer.value = t;
        };

        const changeAst = function(ast) {
            $("#ast").treeview({
                data: ast,
                showBorder: false,
                showTags: true,
                expandIcon: "fa fa-plus",
                collapseIcon: "fa fa-minus",
                onNodeCollapsed: function() {
                    editor.resize();
                },
                onNodeExpanded: function() {
                    editor.resize();
                }
            });
        };

        const changeIl = function(il) {
            $("#il").treeview({
                data: il,
                showBorder: false,
                showTags: true,
                expandIcon: "fa fa-plus",
                collapseIcon: "fa fa-minus",
                onNodeCollapsed: function() {
                    editor.resize();
                },
                onNodeExpanded: function() {
                    editor.resize();
                }
            });
        };

        const changeConsole = function(log) {
            document.getElementById("console").value = log;
        };

        const changeError = function(message) {
            const error = document.getElementById("error");
            if (message === null) {
                error.innerHTML = "";
            } else {
                error.innerHTML =
                    `<div id="error" class="alert alert-danger alert-dismissible fade show" role="alert"><strong>Error!</strong> ${
                    message
                    }<button type="button" class="close" data-dismiss="alert">&times;</button></div>`;
            }
        };

        editor.setTheme("ace/theme/crimson_editor");
        editor.session.setMode("ace/mode/java");
        editor.session.setUseWrapMode(true);
        editor.renderer.setShowGutter(false);
        editor.session.on("change",
            function(e) {
                if (editor.getValue() === "" || editor.getValue() === hiddenSource.value) return;
                hiddenSource.value = editor.getValue();
                const source = document.getElementById("editor");
                const data = new FormData();
                data.append(source.getAttribute("data-name"), hiddenSource.value);
                data.append(timer.getAttribute("name"), timer.value);
                const request = new XMLHttpRequest();
                request.open("POST", source.getAttribute("data-request-url"), true);
                request.onload = function() {
                    const response = JSON.parse(this.response);
                    changeTimer(response.timer);
                    changeSubmit(response.error);
                    changeAst(response.ast);
                    changeIl(response.il);
                    changeConsole(response.printfnLog);
                    changeError(response.error);
                };
                request.send(data);
            });

        const sampleNames = document.getElementById("sample-names");
        sampleNames.addEventListener("change",
            function() {
                const sampleNamesValue = sampleNames.getAttribute("name") + "=" + sampleNames.value;
                const timerValue = timer.getAttribute("name") + "=" + timer.value;
                const data = sampleNamesValue + "&" + timerValue;
                const request = new XMLHttpRequest();
                request.open("POST", sampleNames.getAttribute("data-request-url"), true);
                request.setRequestHeader("Content-Type", "application/x-www-form-urlencoded; charset=UTF-8");
                request.onload = function() {
                    const response = JSON.parse(this.response);
                    changeTimer(response.timer);
                    changeSubmit(response.error);
                    changeSource(response.source);
                    changeAst(response.ast);
                    changeIl(response.il);
                    changeConsole(response.printfnLog);
                    changeError(response.error);
                };
                sampleNames.value = "";
                request.send(data);
            });

        changeSource(hiddenSource.value);
        changeAst(document.getElementById("hidden-ast").value);
        changeIl(document.getElementById("hidden-il").value);
    };

    return { init: init };
})();