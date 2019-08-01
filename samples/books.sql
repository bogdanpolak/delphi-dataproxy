--
-- Books table - SQLite platform
-- 

BEGIN TRANSACTION;

DROP TABLE IF EXISTS Books;

CREATE TABLE Books (
    ISBN        NVARCHAR (20)   PRIMARY KEY
                                NOT NULL,
    Title       NVARCHAR (100)  NOT NULL,
    Authors     NVARCHAR (100),
    Status      NVARCHAR (15)   NOT NULL,
    ReleseDate  DATE,
    Pages       INTEGER,
    Price       DECIMAL (12, 2),
    Currency    NVARCHAR (10),
    Imported    DATETIME,
    Description NVARCHAR (2000) 
);

INSERT INTO Books (
                      ISBN,
                      Title,
                      Authors,
                      Status,
                      ReleseDate,
                      Pages,
                      Price,
                      Currency,
                      Imported,
                      Description
                  )
                  VALUES (
                      '978-0201633610',
                      'Design Patterns: Elements of Reusable Object-Oriented Software',
                      'Erich Gamma, Richard Helm, Ralph Johnson, John Vlissides',
                      'on-shelf',
                      '1994-11-01',
                      395,
                      54.9,
                      'USD',
                      '2017-06-30',
                      'Modern classic in the literature of object-oriented development. Offering timeless and elegant solutions to common problems in software design. It describes 23 patterns for object creation, composing them, and coordinating control flow between them.'
                  );

INSERT INTO Books (
                      ISBN,
                      Title,
                      Authors,
                      Status,
                      ReleseDate,
                      Pages,
                      Price,
                      Currency,
                      Imported,
                      Description
                  )
                  VALUES (
                      '978-0201485677',
                      'Refactoring: Improving the Design of Existing Code',
                      'Martin Fowler, Kent Beck, John Brant, William Opdyke, Don Roberts',
                      'on-shelf',
                      '1999-07-01',
                      464,
                      52.98,
                      'USD',
                      '2017-07-09',
                      'Book shows how refactoring can make object-oriented code simpler and easier to maintain. Provides a catalog of tips for improving code.'
                  );

INSERT INTO Books (
                      ISBN,
                      Title,
                      Authors,
                      Status,
                      ReleseDate,
                      Pages,
                      Price,
                      Currency,
                      Imported,
                      Description
                  )
                  VALUES (
                      '978-0131177055',
                      'Working Effectively with Legacy Code',
                      'Michael Feathers',
                      'on-shelf',
                      '2004-10-01',
                      464,
                      52.69,
                      'USD',
                      '2017-08-07',
                      'This book describes a set of disciplines, concepts, and attitudes that you will carry with you for the rest of your career and that will help you to turn systems that gradually degrade into systems that gradually improve.'
                  );

INSERT INTO Books (
                      ISBN,
                      Title,
                      Authors,
                      Status,
                      ReleseDate,
                      Pages,
                      Price,
                      Currency,
                      Imported,
                      Description
                  )
                  VALUES (
                      '978-0321127426',
                      'Patterns of Enterprise Application Architecture',
                      'Martin Fowler',
                      'avaliable',
                      '2002-11-01',
                      560,
                      55.99,
                      'USD',
                      '2017-08-20',
                      'This book is written in direct response to the stiff challenges that face enterprise application developers. Author distills over forty recurring solutions into patterns. Indispensable handbook of solutions that are applicable to any enterprise application platform.'
                  );

INSERT INTO Books (
                      ISBN,
                      Title,
                      Authors,
                      Status,
                      ReleseDate,
                      Pages,
                      Price,
                      Currency,
                      Imported,
                      Description
                  )
                  VALUES (
                      '978-1849689121',
                      'Applied Architecture Patterns on the Microsoft Platform (Second Edition)',
                      'Andre Dovgal, Dmitri Olechko, Gregor Noriskin',
                      'avaliable',
                      '2014-07-01',
                      456,
                      23.24,
                      'EUR',
                      '2017-09-18',
                      'Work with various Microsoft technologies using Applied Architecture Patterns'
                  );

INSERT INTO Books (
                      ISBN,
                      Title,
                      Authors,
                      Status,
                      ReleseDate,
                      Pages,
                      Price,
                      Currency,
                      Imported,
                      Description
                  )
                  VALUES (
                      '978-0735619678',
                      'Code Complete: A Practical Handbook of Software Construction',
                      'Steve McConnell',
                      'avaliable',
                      '2004-06-01',
                      960,
                      40.97,
                      'USD',
                      '2017-09-18',
                      'Author synthesizes the most effective techniques and must-know principles into clear, pragmatic book which will inform and stimulate your thinking and help you build the highest quality software'
                  );

INSERT INTO Books (
                      ISBN,
                      Title,
                      Authors,
                      Status,
                      ReleseDate,
                      Pages,
                      Price,
                      Currency,
                      Imported,
                      Description
                  )
                  VALUES (
                      '978-0132350884',
                      'Clean Code: A Handbook of Agile Software Craftsmanship',
                      'Robert C. Martin',
                      'on-shelf',
                      '2008-08-01',
                      464,
                      47.49,
                      'USD',
                      '2017-09-19',
                      'Best agile practices of cleaning code “on the fly” that will instill within you the values of a software craftsman and make you a better programmer—but only if you work at it.'
                  );

INSERT INTO Books (
                      ISBN,
                      Title,
                      Authors,
                      Status,
                      ReleseDate,
                      Pages,
                      Price,
                      Currency,
                      Imported,
                      Description
                  )
                  VALUES (
                      '978-1941266106',
                      'More Coding in Delphi',
                      'Nick Hodges',
                      'on-shelf',
                      '2015-12-01',
                      246,
                      25.99,
                      'USD',
                      '2017-10-01',
                      'Picks up where previous "Coding in Delphi" left of, continuing to illustrate good, sound coding techniques including design patterns and principles'
                  );

INSERT INTO Books (
                      ISBN,
                      Title,
                      Authors,
                      Status,
                      ReleseDate,
                      Pages,
                      Price,
                      Currency,
                      Imported,
                      Description
                  )
                  VALUES (
                      '978-1941266038',
                      'Coding in Delphi',
                      'Nick Hodges',
                      'on-shelf',
                      '2014-04-01',
                      236,
                      24.99,
                      'USD',
                      '2017-10-05',
                      'All about writing Delphi code. It''s just about how to use the language in the most effective way to write clean, testable, maintainable Delphi code'
                  );

INSERT INTO Books (
                      ISBN,
                      Title,
                      Authors,
                      Status,
                      ReleseDate,
                      Pages,
                      Price,
                      Currency,
                      Imported,
                      Description
                  )
                  VALUES (
                      '978-1785287428',
                      'Delphi Cookbook - Second Edition',
                      'Daniele Teti',
                      'avaliable',
                      '2016-06-01',
                      470,
                      30.13,
                      'EUR',
                      '2017-10-18',
                      'Over 60 hands-on recipes to help you master the power of Delphi for cross-platform and mobile development on multiple platforms'
                  );

INSERT INTO Books (
                      ISBN,
                      Title,
                      Authors,
                      Status,
                      ReleseDate,
                      Pages,
                      Price,
                      Currency,
                      Imported,
                      Description
                  )
                  VALUES (
                      '978-1786466150',
                      '.NET Design Patterns',
                      'Praseed Pai, Shine Xavier',
                      'avaliable',
                      '2017-01-01',
                      314,
                      26.69,
                      'EUR',
                      '2017-10-27',
                      'Explore the world of .NET design patterns and bring the benefits that the right patterns can offer to your toolkit today'
                  );

INSERT INTO Books (
                      ISBN,
                      Title,
                      Authors,
                      Status,
                      ReleseDate,
                      Pages,
                      Price,
                      Currency,
                      Imported,
                      Description
                  )
                  VALUES (
                      '978-1786460165',
                      'Expert Delphi',
                      'Paweł Głowacki',
                      'avaliable',
                      '2017-06-01',
                      506,
                      32.71,
                      'EUR',
                      '2017-12-12',
                      'Become a developer superhero and build stunning cross-platform apps with Delphi'
                  );

INSERT INTO Books (
                      ISBN,
                      Title,
                      Authors,
                      Status,
                      ReleseDate,
                      Pages,
                      Price,
                      Currency,
                      Imported,
                      Description
                  )
                  VALUES (
                      '978-1546391272',
                      'Delphi in Depth: FireDAC',
                      'Cary Jensen Ph.D',
                      'avaliable',
                      '2017-05-01',
                      556,
                      52.43,
                      'EUR',
                      '2017-12-21',
                      'Learn how to connect to a wide variety of databases, optimize your connection configurations, the power of persisted datasets, create flexible queries using macros and FireDAC scalar functions, achieve blazing performance with Array DML, Master the art of cached updates'
                  );

INSERT INTO Books (
                      ISBN,
                      Title,
                      Authors,
                      Status,
                      ReleseDate,
                      Pages,
                      Price,
                      Currency,
                      Imported,
                      Description
                  )
                  VALUES (
                      '978-1941266229',
                      'Dependency Injection In Delphi',
                      'Nick Hodges',
                      'avaliable',
                      '2017-02-01',
                      132,
                      18.18,
                      'USD',
                      '2017-12-23',
                      'Covers Dependency Injection, you''ll learn about Constructor Injection, Property Injection, and Method Injection and about the right and wrong way to use it'
                  );

INSERT INTO Books (
                      ISBN,
                      Title,
                      Authors,
                      Status,
                      ReleseDate,
                      Pages,
                      Price,
                      Currency,
                      Imported,
                      Description
                  )
                  VALUES (
                      '978-1788625456',
                      'Delphi High Performance',
                      'Primož Gabrijelčič',
                      'avaliable',
                      '2018-02-01',
                      336,
                      25.83,
                      'EUR',
                      '2017-12-30',
                      'Build fast, scalable, and high performing applications with Delphi'
                  );

INSERT INTO Books (
                      ISBN,
                      Title,
                      Authors,
                      Status,
                      ReleseDate,
                      Pages,
                      Price,
                      Currency,
                      Imported,
                      Description
                  )
                  VALUES (
                      '978-1788621304',
                      'Delphi Cookbook - Third Edition',
                      'Daniele Spinetti, Daniele Teti',
                      'avaliable',
                      '2018-07-01',
                      668,
                      30.13,
                      'EUR',
                      '2018-03-24',
                      'Quickly learn and employ practical recipes for developing real-world, cross-platform applications using Delphi'
                  );

INSERT INTO Books (
                      ISBN,
                      Title,
                      Authors,
                      Status,
                      ReleseDate,
                      Pages,
                      Price,
                      Currency,
                      Imported,
                      Description
                  )
                  VALUES (
                      '978-1788625258',
                      'Hands-On Design Patterns with C# and .NET Core',
                      'Gaurav Aroraa, Jeffrey Chilberto',
                      'cooming-soon',
                      '1899-12-30',
                      437,
                      25.83,
                      'EUR',
                      '2019-02-16 22:20:16.493',
                      'Build effective applications in C# and .NET Core by using proven programming practices and design techniques.'
                  );

INSERT INTO Books (
                      ISBN,
                      Title,
                      Authors,
                      Status,
                      ReleseDate,
                      Pages,
                      Price,
                      Currency,
                      Imported,
                      Description
                  )
                  VALUES (
                      '978-1788624176',
                      'Delphi GUI Programming with FireMonkey',
                      'Andrea Magni',
                      'cooming-soon',
                      '1899-12-30',
                      437,
                      29.27,
                      'EUR',
                      '2019-02-16 22:20:16.496',
                      'Master the techniques to build interesting cross platform GUI applications with FMX'
                  );

INSERT INTO Books (
                      ISBN,
                      Title,
                      Authors,
                      Status,
                      ReleseDate,
                      Pages,
                      Price,
                      Currency,
                      Imported,
                      Description
                  )
                  VALUES (
                      '978-1789343243',
                      'Hands-On Design Patterns with Delphi',
                      'Primož Gabrijelčič',
                      'on-shelf',
                      '2019-02-27',
                      476,
                      35.99,
                      'EUR',
                      '2019-02-16 22:20:16.497',
                      'Build scalable projects via exploring design patterns in Delphi'
                  );

INSERT INTO Books (
                      ISBN,
                      Title,
                      Authors,
                      Status,
                      ReleseDate,
                      Pages,
                      Price,
                      Currency,
                      Imported,
                      Description
                  )
                  VALUES (
                      '978-1788834094',
                      'Hands-On Domain Driven Design with .NET',
                      'Alexey Zimarev',
                      'avaliable',
                      '2019-04-29',
                      446,
                      31.99,
                      'EUR',
                      '2019-02-16 22:20:16.498',
                      'Learn to solve complex business problems by understanding users better, finding the right problem to solve, and building lean event-driven systems to give your customers what they really want.'
                  );


COMMIT TRANSACTION;
