program BooksDataProxy;

uses
  Vcl.Forms,
  Form.Main in 'Form.Main.pas' {Form1},
  Data.Proxy.Book in 'Data.Proxy.Book.pas',
  Data.Mock.Book in 'Data.Mock.Book.pas',
  Data.DataProxy in '..\..\src\Data.DataProxy.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
