program CodeEvolutionDemo;

uses
  Vcl.Forms,
  Form.Main in 'Form.Main.pas' {FormMain},
  Comp.Generator.DataProxy in '..\..\src\Comp.Generator.DataProxy.pas',
  Data.DataProxy in '..\..\src\Data.DataProxy.pas',
  Proxy.Books in 'Proxy.Books.pas',
  Model.Books in 'Model.Books.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
