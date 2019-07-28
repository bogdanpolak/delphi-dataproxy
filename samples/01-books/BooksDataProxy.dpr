program BooksDataProxy;

uses
  Vcl.Forms,
  Form.Main in 'Form.Main.pas' {Form1},
  Data.Proxy.Book in 'Data.Proxy.Book.pas',
  Data.DataProxy in '..\base\Data.DataProxy.pas',
  Data.DataProxy.Factory in '..\base\Data.DataProxy.Factory.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
