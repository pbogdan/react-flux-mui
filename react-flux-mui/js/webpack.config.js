module.exports = {
    entry: "./index",
    output: {
        path: ".",
        filename: "react-flux-mui.js"
    },
    module: {
        loaders: [
            { test: /\.jsx$/, loader: 'babel-loader' }
        ]
    }
};
