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

Classic approach (`TDataSet-s` placed on the `TDataModule`) almost prevents proper (object oriented) organization of the project: which is bundling the data with the behavior that uses it. A Data Proxy object organizes domain logic with one class per TDataSet query (eg. SELECT command).

This is a pragmatic approach that is fairly easy to implement and quick to deploy. Migration process can be partially automated.

- **Pros**:
   - Better project organization
   - Easier to find code to be fixed or developed
   - Allows to implement unit tests with a memory datasets and with mocks
   - Allows to modernize the project architecture (SOLID principles, GoF patterns)
   - Encourages you to write clean code
   - Path towards the domain model (use ORM framework)
   - Opening to the Test-Driven Development (TDD)
   - Facilitates verification of new code through peer reviews
   - Reduces the number of dependencies and the level of project complexity
   - Allows you to pay off technical debt
