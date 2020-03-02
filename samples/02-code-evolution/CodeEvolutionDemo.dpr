program CodeEvolutionDemo;

uses
  Vcl.Forms,
  Form.MainBefore in 'Form.MainBefore.pas' {Form1},
  Comp.Generator.DataProxy in '..\..\src\Comp.Generator.DataProxy.pas',
  Data.DataProxy in '..\..\src\Data.DataProxy.pas',
  Proxy.Books in 'Proxy.Books.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
