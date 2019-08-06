# TDataSet vs TDataSetProxy

This document is to help you understand why the TDataSetProxy pattern was created. Contains a comparison of the classic approach based on TDataSet component with the proxy variant.


## Classic approach

Working with RDBMS (SQL servers) in Delphi looks to be very productive and simple. You're doing a simple scenario: drop a TQuery component, enter SQL command, set Active and you are done (almost). After few minutes you can see data stored on the server in a interactive DBGrid control. During the development of such RAD projects over time, the code becomes more and more difficult to understand and confusing. Managers and developers lose control over the closing time of even simple tasks.

## Why using proxy?

TBD ...