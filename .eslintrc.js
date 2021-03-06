// http://eslint.org/docs/user-guide/configuring

module.exports = {
	root: true,
	parser: 'babel-eslint',
	parserOptions: {
		sourceType: 'module'
	},
	env: {
		browser: false,
		node: true,
		es6: true
	},
	// https://github.com/standard/standard/blob/master/docs/RULES-en.md
	extends: 'standard',
	// required to lint *.vue files
	plugins: [
		'html'
	],
	// add your custom rules here
	'rules': {
		"indent": ["error", "tab"],
		"linebreak-style": ["error", "windows"],
		"no-tabs": 0,
		"comma-dangle": ["error", "never"],
		// Disable EsLint Error
		"space-before-function-paren": 1,
		"one-var": 1,
		"eqeqeq": 1,
		"no-unused-vars": 1,
		"semi": 0,
		// allow paren-less arrow functions
		'arrow-parens': 0,
		// allow async-await
		'generator-star-spacing': 0,
		// allow debugger during development
		'no-debugger': process.env.NODE_ENV === 'production' ? 2 : 0
	},
	globals: {
		App: true,
		Page: true,
		wx: true,
		getApp: true,
		getPage: true,
		requirePlugin: true
	}
};
