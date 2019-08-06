# TDataSet vs TDataSetProxy

This document is to help you understand why the TDataSetProxy pattern was created. Contains a comparison of the classic approach based on TDataSet component with the proxy variant.


## Classic approach

Working with RDBMS (SQL servers) in Delphi looks to be very productive and simple. You're doing a simple scenario: drop a TQuery component, enter SQL command, set Active and you are done (almost). After few minutes you can see data stored on the server in a interactive DBGrid control. During the development of such RAD projects over time, the code becomes more and more difficult to understand and confusing. Managers and developers lose control over the closing time of even simple tasks.

- **Pros**:
   - Intuitive
   - Easy to learn
   - Productive
   - Advance performance optimization
   - Productive (in simple projects = less than ~10K lines of code)
- **Cons**:
   - Messy code
   - Almost no architectural design
   - Massive copy-paste development
   - Mixing layers
      - Manipulation of user controls along with business logic and data in a single method (procedure or event)
   - Difficult to debug
   - Difficult to understand
   - Lack of control and visibility
   - High technical debt
   - Stagnation and demotivation
      - Developers aren’t motivated to learn, improve and change
   - Difficult to separate and reuse code
   - Difficult to introduce unit tests

## Why using proxy?

TBD ...