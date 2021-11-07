const path = require('path');

const {merge} = require('webpack-merge');
const common = require('./webpack.common.js');


const dev = {
    mode: 'development',
    devServer: {
        port: 3000,
        hot: true,
        static: path.join(__dirname, "../src/assets"),
        client: {
            overlay: false
        }
    },
};

module.exports = env => {
    const withDebug = !env.nodebug;
    return merge(common(withDebug), dev);
}
