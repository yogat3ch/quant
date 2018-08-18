# Quant
Note: Some files that include personal portfolio data are not included in the push. These lines can be removed, and you can use a vector of the symbols you would like to work with for variable 'Positions.v'. Other sourced files are included in the template - these lines can also be removed as they are not used.
<ul style="list-style-type: disc;">
	<li>Technical.Rmd is the primary document that contains the core functionality.</li>
	<li>iex.R contains support functions for requesting data</li>
	<li>TechnicalIndicatorAutomation.Rmd is intended to hold the machine learning exploration should it become too large and cumbersome for a single file.</li>
</ul>

## Purpose
<p>The purpose(s) of the quant project are to:
<ol style="list-style-type: lower-alpha;">
  <li>To create a simplified dashboard for quantitative evaluation of prospective stocks to be added to a portfolio.</li>
  <li>To use linear models for a simple comparative view of stock performance</li>
  <li>To explore for reliable signals in various derived response variables using machine learning algorithms with various well-documented indicators as independent variables. <em>(Parabolic Stop-and-Reverse, ADX, MACD etc.)</em> </li>
  <li>To develop a stock discovery tool with the KNN algorithm to match well-performing stocks with those that have similar performance and/or vary colinearly.</li>
</ol>
</p>

## Features
### Financial Stats
<ol>
  <li>Intraday data requests from the free IEX API (document sources iex.R for functions)</li>
  <li>Combination of all relevant financial stats into a single dataframe for comparison</li>
  <li>A datatable with linear regression analysis outputs and risk evaluation metrics</li>
</ol>

### Machine Learning Exploration for Reliable Signals
<ol>
	<li>A meta-function for adding the following response variables to a list of intraday data for various stocks:
		<ul>
			<li>Percent change over various timeperiods (as specified in the timeperiods variable)</li>
			<li>Percent of days over the time period that showed gains</li>
			<li>The standard deviation over the time period</li>
		</ul></li>
</ol>

