{
    // Use IntelliSense to learn about possible attributes.
    // Hover to view descriptions of existing attributes.
    // For more information, visit: https://go.microsoft.com/fwlink/?linkid=830387
    "version": "0.2.0",
    "configurations": [
        
        {
            "comments": [
                "Full launch.json configuration details can be found here:",
                "https://code.visualstudio.com/docs/cpp/launch-json-reference"
            ],
            "name": "oneAPI C++: Launch & Break",
            "type": "cppdbg",
            "request": "launch",
            "preLaunchTask": "compile",
            "postDebugTask": "",
            "program": "${workspaceFolder}/main.exe",
            "args": [],
            "stopAtEntry": true,
            "cwd": "${workspaceFolder}",
            "environment": [
                {
                    "name": "ZET_ENABLE_PROGRAM_DEBUGGING",
                    "value": "1"
                },
                {
                    "name": "IGC_EnableGTLocationDebugging",
                    "value": "1"
                }
            ],
            "externalConsole": false,
            "MIMode": "gdb",
            "miDebuggerPath": "gdb-oneapi",
            "setupCommands": [
                {
                    "description": "Enable pretty-printing for gdb",
                    "text": "-enable-pretty-printing",
                    "ignoreFailures": true
                },
                {
                    "description": "Disable target async",
                    "text": "set target-async off",
                    "ignoreFailures": true
                }
            ]
        },
        {
            "comments": [
                "Full launch.json configuration details can be found here:",
                "https://code.visualstudio.com/docs/cpp/launch-json-reference"
            ],
            "name": "oneAPI C++: Launch & Run",
            "type": "cppdbg",
            "request": "launch",
            "preLaunchTask": "compile",
            "postDebugTask": "",
            "program": "${workspaceFolder}/main.exe",
            "args": [],
            "stopAtEntry": false,
            "cwd": "${workspaceFolder}",
            "environment": [
                {
                    "name": "ZET_ENABLE_PROGRAM_DEBUGGING",
                    "value": "1"
                },
                {
                    "name": "IGC_EnableGTLocationDebugging",
                    "value": "1"
                }
            ],
            "externalConsole": false,
            "MIMode": "gdb",
            "miDebuggerPath": "gdb-oneapi",
            "setupCommands": [
                {
                    "description": "Enable pretty-printing for gdb",
                    "text": "-enable-pretty-printing",
                    "ignoreFailures": true
                },
                {
                    "description": "Disable target async",
                    "text": "set target-async off",
                    "ignoreFailures": true
                }
            ]
        }
    ]
}