unit Data.Mock.Book;

interface

uses
  System.Classes, System.SysUtils,
  Data.DB,
  FireDAC.Comp.Client;

function CreateMockTableBook(AOwner: TComponent): TFDMemTable;

implementation

function CreateMockTableBook(AOwner: TComponent): TFDMemTable;
var
  ds: TFDMemTable;
begin
  ds := TFDMemTable.Create(AOwner);
  with ds do
  begin
    FieldDefs.Add('ISBN', ftWideString, 20);
    FieldDefs.Add('Title', ftWideString, 100);
    FieldDefs.Add('Authors', ftWideString, 100);
    FieldDefs.Add('Status', ftWideString, 15);
    FieldDefs.Add('ReleseDate', ftDate);
    FieldDefs.Add('Pages', ftInteger);
    with FieldDefs.AddFieldDef do begin
      Name := 'f1';  DataType := ftBCD;  Precision := 12;  Size := 2;
    end;
    FieldDefs.Add('Currency', ftWideString, 10);
    FieldDefs.Add('Imported', ftDateTime);
    FieldDefs.Add('Description', ftWideString, 2000);
    CreateDataSet;
  end;
  with ds do
  begin
    Append;
      FieldByName('ISBN').Value := '978-0201633610';
      FieldByName('Title').Value := 'Design Patterns: Elements of Reusable Object-Oriented Software';
      FieldByName('Authors').Value := 'Erich Gamma, Richard Helm, Ralph Johnson, John Vlissides';
      FieldByName('Status').Value := 'on-shelf';
      FieldByName('ReleseDate').Value := EncodeDate(1994,11,1);
      FieldByName('Pages').Value := 395;
      FieldByName('Price').Value := 54.9;
      FieldByName('Currency').Value := 'USD';
      FieldByName('Imported').Value := EncodeDate(2017,6,30);
      FieldByName('Description').Value :=
        'Modern classic in the literature of object-oriented development. Off'+
        'ering timeless and elegant solutions to common problems in software '+
        'design. It describes 23 patterns for object creation, composing them'+
        ', and coordinating control flow between them.';
    Post;
  end;
  with ds do
  begin
    Append;
      FieldByName('ISBN').Value := '978-0201485677';
      FieldByName('Title').Value := 'Refactoring: Improving the Design of Existing Code';
      FieldByName('Authors').Value := 'Martin Fowler, Kent Beck, John Brant, William Opdyke, Don Roberts';
      FieldByName('Status').Value := 'on-shelf';
      FieldByName('ReleseDate').Value := EncodeDate(1999,7,1);
      FieldByName('Pages').Value := 464;
      FieldByName('Price').Value := 52.98;
      FieldByName('Currency').Value := 'USD';
      FieldByName('Imported').Value := EncodeDate(2017,7,9);
      FieldByName('Description').Value :=
        'Book shows how refactoring can make object-oriented code simpler and'+
        ' easier to maintain. Provides a catalog of tips for improving code.';
    Post;
  end;
  with ds do
  begin
    Append;
      FieldByName('ISBN').Value := '978-0131177055';
      FieldByName('Title').Value := 'Working Effectively with Legacy Code';
      FieldByName('Authors').Value := 'Michael Feathers';
      FieldByName('Status').Value := 'on-shelf';
      FieldByName('ReleseDate').Value := EncodeDate(2004,10,1);
      FieldByName('Pages').Value := 464;
      FieldByName('Price').Value := 52.69;
      FieldByName('Currency').Value := 'USD';
      FieldByName('Imported').Value := EncodeDate(2017,8,7);
      FieldByName('Description').Value :=
        'This book describes a set of disciplines, concepts, and attitudes th'+
        'at you will carry with you for the rest of your career and that will'+
        ' help you to turn systems that gradually degrade into systems that g'+
        'radually improve.';
    Post;
    Append;
      FieldByName('ISBN').Value := '978-0321127426';
      FieldByName('Title').Value := 'Patterns of Enterprise Application Architecture';
      FieldByName('Authors').Value := 'Martin Fowler';
      FieldByName('Status').Value := 'avaliable';
      FieldByName('ReleseDate').Value := EncodeDate(2002,11,1);
      FieldByName('Pages').Value := 560;
      FieldByName('Price').Value := 55.99;
      FieldByName('Currency').Value := 'USD';
      FieldByName('Imported').Value := EncodeDate(2017,8,20);
      FieldByName('Description').Value :=
        'This book is written in direct response to the stiff challenges that'+
        ' face enterprise application developers. Author distills over forty '+
        'recurring solutions into patterns. Indispensable handbook of solutio'+
        'ns that are applicable to any enterprise application platform.';
    Post;
    Append;
      FieldByName('ISBN').Value := '978-1849689121';
      FieldByName('Title').Value :=
        'Applied Architecture Patterns on the Microsoft Platform (Second Edit'+
        'ion)';
      FieldByName('Authors').Value := 'Andre Dovgal, Dmitri Olechko, Gregor Noriskin';
      FieldByName('Status').Value := 'avaliable';
      FieldByName('ReleseDate').Value := EncodeDate(2014,7,1);
      FieldByName('Pages').Value := 456;
      FieldByName('Price').Value := 23.24;
      FieldByName('Currency').Value := 'EUR';
      FieldByName('Imported').Value := EncodeDate(2017,9,18);
      FieldByName('Description').Value :=
        'Work with various Microsoft technologies using Applied Architecture '+
        'Patterns';
    Post;
    Append;
      FieldByName('ISBN').Value := '978-0735619678';
      FieldByName('Title').Value := 'Code Complete: A Practical Handbook of Software Construction';
      FieldByName('Authors').Value := 'Steve McConnell';
      FieldByName('Status').Value := 'avaliable';
      FieldByName('ReleseDate').Value := EncodeDate(2004,6,1);
      FieldByName('Pages').Value := 960;
      FieldByName('Price').Value := 40.97;
      FieldByName('Currency').Value := 'USD';
      FieldByName('Imported').Value := EncodeDate(2017,9,18);
      FieldByName('Description').Value :=
        'Author synthesizes the most effective techniques and must-know princ'+
        'iples into clear, pragmatic book which will inform and stimulate you'+
        'r thinking and help you build the highest quality software';
    Post;
    Append;
      FieldByName('ISBN').Value := '978-0132350884';
      FieldByName('Title').Value := 'Clean Code: A Handbook of Agile Software Craftsmanship';
      FieldByName('Authors').Value := 'Robert C. Martin';
      FieldByName('Status').Value := 'on-shelf';
      FieldByName('ReleseDate').Value := EncodeDate(2008,8,1);
      FieldByName('Pages').Value := 464;
      FieldByName('Price').Value := 47.49;
      FieldByName('Currency').Value := 'USD';
      FieldByName('Imported').Value := EncodeDate(2017,9,19);
      FieldByName('Description').Value :=
        'Best agile practices of cleaning code “on the fly” that will instill'+
        ' within you the values of a software craftsman and make you a better'+
        ' programmer—but only if you work at it.';
    Post;
    Append;
      FieldByName('ISBN').Value := '978-1941266106';
      FieldByName('Title').Value := 'More Coding in Delphi';
      FieldByName('Authors').Value := 'Nick Hodges';
      FieldByName('Status').Value := 'on-shelf';
      FieldByName('ReleseDate').Value := EncodeDate(2015,12,1);
      FieldByName('Pages').Value := 246;
      FieldByName('Price').Value := 25.99;
      FieldByName('Currency').Value := 'USD';
      FieldByName('Imported').Value := EncodeDate(2017,10,1);
      FieldByName('Description').Value :=
        'Picks up where previous "Coding in Delphi" left of, continuing to il'+
        'lustrate good, sound coding techniques including design patterns and'+
        ' principles';
    Post;
    Append;
      FieldByName('ISBN').Value := '978-1941266038';
      FieldByName('Title').Value := 'Coding in Delphi';
      FieldByName('Authors').Value := 'Nick Hodges';
      FieldByName('Status').Value := 'on-shelf';
      FieldByName('ReleseDate').Value := EncodeDate(2014,4,1);
      FieldByName('Pages').Value := 236;
      FieldByName('Price').Value := 24.99;
      FieldByName('Currency').Value := 'USD';
      FieldByName('Imported').Value := EncodeDate(2017,10,5);
      FieldByName('Description').Value :=
        'All about writing Delphi code. It''s just about how to use the langu'+
        'age in the most effective way to write clean, testable, maintainable'+
        ' Delphi code';
    Post;
    Append;
      FieldByName('ISBN').Value := '978-1785287428';
      FieldByName('Title').Value := 'Delphi Cookbook - Second Edition';
      FieldByName('Authors').Value := 'Daniele Teti';
      FieldByName('Status').Value := 'avaliable';
      FieldByName('ReleseDate').Value := EncodeDate(2016,6,1);
      FieldByName('Pages').Value := 470;
      FieldByName('Price').Value := 30.13;
      FieldByName('Currency').Value := 'EUR';
      FieldByName('Imported').Value := EncodeDate(2017,10,18);
      FieldByName('Description').Value :=
        'Over 60 hands-on recipes to help you master the power of Delphi for '+
        'cross-platform and mobile development on multiple platforms';
    Post;
    Append;
      FieldByName('ISBN').Value := '978-1786466150';
      FieldByName('Title').Value := '.NET Design Patterns';
      FieldByName('Authors').Value := 'Praseed Pai, Shine Xavier';
      FieldByName('Status').Value := 'avaliable';
      FieldByName('ReleseDate').Value := EncodeDate(2017,1,1);
      FieldByName('Pages').Value := 314;
      FieldByName('Price').Value := 26.69;
      FieldByName('Currency').Value := 'EUR';
      FieldByName('Imported').Value := EncodeDate(2017,10,27);
      FieldByName('Description').Value :=
        'Explore the world of .NET design patterns and bring the benefits tha'+
        't the right patterns can offer to your toolkit today';
    Post;
    Append;
      FieldByName('ISBN').Value := '978-1786460165';
      FieldByName('Title').Value := 'Expert Delphi';
      FieldByName('Authors').Value := 'Pawe³ G³owacki';
      FieldByName('Status').Value := 'avaliable';
      FieldByName('ReleseDate').Value := EncodeDate(2017,6,1);
      FieldByName('Pages').Value := 506;
      FieldByName('Price').Value := 32.71;
      FieldByName('Currency').Value := 'EUR';
      FieldByName('Imported').Value := EncodeDate(2017,12,12);
      FieldByName('Description').Value :=
        'Become a developer superhero and build stunning cross-platform apps '+
        'with Delphi';
    Post;
    Append;
      FieldByName('ISBN').Value := '978-1546391272';
      FieldByName('Title').Value := 'Delphi in Depth: FireDAC';
      FieldByName('Authors').Value := 'Cary Jensen Ph.D';
      FieldByName('Status').Value := 'avaliable';
      FieldByName('ReleseDate').Value := EncodeDate(2017,5,1);
      FieldByName('Pages').Value := 556;
      FieldByName('Price').Value := 52.43;
      FieldByName('Currency').Value := 'EUR';
      FieldByName('Imported').Value := EncodeDate(2017,12,21);
      FieldByName('Description').Value :=
        'Learn how to connect to a wide variety of databases, optimize your c'+
        'onnection configurations, the power of persisted datasets, create fl'+
        'exible queries using macros and FireDAC scalar functions, achieve bl'+
        'azing performance with Array DML, Master the art of cached updates';
    Post;
    Append;
      FieldByName('ISBN').Value := '978-1941266229';
      FieldByName('Title').Value := 'Dependency Injection In Delphi';
      FieldByName('Authors').Value := 'Nick Hodges';
      FieldByName('Status').Value := 'avaliable';
      FieldByName('ReleseDate').Value := EncodeDate(2017,2,1);
      FieldByName('Pages').Value := 132;
      FieldByName('Price').Value := 18.18;
      FieldByName('Currency').Value := 'USD';
      FieldByName('Imported').Value := EncodeDate(2017,12,23);
      FieldByName('Description').Value :=
        'Covers Dependency Injection, you''ll learn about Constructor Injecti'+
        'on, Property Injection, and Method Injection and about the right and'+
        ' wrong way to use it';
    Post;
    Append;
      FieldByName('ISBN').Value := '978-1788625456';
      FieldByName('Title').Value := 'Delphi High Performance';
      FieldByName('Authors').Value := 'Primož Gabrijelèiè';
      FieldByName('Status').Value := 'avaliable';
      FieldByName('ReleseDate').Value := EncodeDate(2018,2,1);
      FieldByName('Pages').Value := 336;
      FieldByName('Price').Value := 25.83;
      FieldByName('Currency').Value := 'EUR';
      FieldByName('Imported').Value := EncodeDate(2017,12,30);
      FieldByName('Description').Value := 'Build fast, scalable, and high performing applications with Delphi';
    Post;
    Append;
      FieldByName('ISBN').Value := '978-1788621304';
      FieldByName('Title').Value := 'Delphi Cookbook - Third Edition';
      FieldByName('Authors').Value := 'Daniele Spinetti, Daniele Teti';
      FieldByName('Status').Value := 'avaliable';
      FieldByName('ReleseDate').Value := EncodeDate(2018,7,1);
      FieldByName('Pages').Value := 668;
      FieldByName('Price').Value := 30.13;
      FieldByName('Currency').Value := 'EUR';
      FieldByName('Imported').Value := EncodeDate(2018,3,24);
      FieldByName('Description').Value :=
        'Quickly learn and employ practical recipes for developing real-world'+
        ', cross-platform applications using Delphi';
    Post;
    Append;
      FieldByName('ISBN').Value := '978-1788625258';
      FieldByName('Title').Value := 'Hands-On Design Patterns with C# and .NET Core';
      FieldByName('Authors').Value := 'Gaurav Aroraa, Jeffrey Chilberto';
      FieldByName('Status').Value := 'cooming-soon';
      FieldByName('ReleseDate').Value := EncodeDate(1899,12,30);
      FieldByName('Pages').Value := 437;
      FieldByName('Price').Value := 25.83;
      FieldByName('Currency').Value := 'EUR';
      FieldByName('Imported').Value := EncodeDate(2019,2,16)+EncodeTime(22,20,16,493);
      FieldByName('Description').Value :=
        'Build effective applications in C# and .NET Core by using proven pro'+
        'gramming practices and design techniques.';
    Post;
    Append;
      FieldByName('ISBN').Value := '978-1788624176';
      FieldByName('Title').Value := 'Delphi GUI Programming with FireMonkey';
      FieldByName('Authors').Value := 'Andrea Magni';
      FieldByName('Status').Value := 'cooming-soon';
      FieldByName('ReleseDate').Value := EncodeDate(1899,12,30);
      FieldByName('Pages').Value := 437;
      FieldByName('Price').Value := 29.27;
      FieldByName('Currency').Value := 'EUR';
      FieldByName('Imported').Value := EncodeDate(2019,2,16)+EncodeTime(22,20,16,496);
      FieldByName('Description').Value :=
        'Master the techniques to build interesting cross platform GUI applic'+
        'ations with FMX';
    Post;
    Append;
      FieldByName('ISBN').Value := '978-1789343243';
      FieldByName('Title').Value := 'Hands-On Design Patterns with Delphi';
      FieldByName('Authors').Value := 'Primož Gabrijelèiè';
      FieldByName('Status').Value := 'on-shelf';
      FieldByName('ReleseDate').Value := EncodeDate(2019,2,27);
      FieldByName('Pages').Value := 476;
      FieldByName('Price').Value := 35.99;
      FieldByName('Currency').Value := 'EUR';
      FieldByName('Imported').Value := EncodeDate(2019,2,16)+EncodeTime(22,20,16,497);
      FieldByName('Description').Value := 'Build scalable projects via exploring design patterns in Delphi';
    Post;
    Append;
      FieldByName('ISBN').Value := '978-1788834094';
      FieldByName('Title').Value := 'Hands-On Domain Driven Design with .NET';
      FieldByName('Authors').Value := 'Alexey Zimarev';
      FieldByName('Status').Value := 'avaliable';
      FieldByName('ReleseDate').Value := EncodeDate(2019,4,29);
      FieldByName('Pages').Value := 446;
      FieldByName('Price').Value := 31.99;
      FieldByName('Currency').Value := 'EUR';
      FieldByName('Imported').Value := EncodeDate(2019,2,16)+EncodeTime(22,20,16,498);
      FieldByName('Description').Value :=
        'Learn to solve complex business problems by understanding users bett'+
        'er, finding the right problem to solve, and building lean event-driv'+
        'en systems to give your customers what they really want.';
    Post;
  end;
  Result := ds;
end;

end.
