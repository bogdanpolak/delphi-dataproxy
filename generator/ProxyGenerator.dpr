program ProxyGenerator;

uses
  Vcl.Forms,
  Form.Main in 'Form.Main.pas' {FormMain},
  Dialog.SelectDefinition in 'Dialog.SelectDefinition.pas' {DialogSelectDefinition},
  DataModule.Main in 'DataModule.Main.pas' {DataModule1: TDataModule},
  Helper.TApplication in 'Helper.TApplication.pas',
  Comp.Proxy.CodeGenerator in 'Comp.Proxy.CodeGenerator.pas',
  App.AppInfo in 'App.AppInfo.pas',
  Helper.TDBGrid in 'Helper.TDBGrid.pas',
  Dialog.QueryBuilder in 'Dialog.QueryBuilder.pas' {DialogQueryBuilder},
  Helper.TField in 'Helper.TField.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TDataModule1, DataModule1);
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
