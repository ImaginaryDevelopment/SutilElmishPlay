// Note this only includes basic configuration for development mode.
// For a more comprehensive configuration check:
// https://github.com/fable-compiler/webpack-config-template

const path = require("path");

module.exports = {
    mode: "development",
    entry: "./src/App/App.fs.js",
    output: {
        path: path.join(__dirname, "./public"),
        filename: "bundle.js",
    },
    devServer: {
        publicPath: "/",
        contentBase: "./public",
        port: 8080,
    },
    module: {
        rules: [
            {
                test: /\.(sass|scss|css)$/,
                oneOf: [
                    {
                        test: /\.css$/,
                        use: [
                            // MiniCssExtractPlugin.loader,
                            'css-loader'
                        ]
                    }
                ]
            }
        ]
    }
}
