program ProxyGenerator;

uses
  Vcl.Forms,
  Helper.TApplication in 'helpers\Helper.TApplication.pas',
  Helper.TDBGrid in 'helpers\Helper.TDBGrid.pas',
  Helper.TField in 'helpers\Helper.TField.pas',
  Helper.TStrings in 'helpers\Helper.TStrings.pas',
  Helper.TFDCustomManager in 'helpers\Helper.TFDCustomManager.pas',
  Helper.TFDConnection in 'helpers\Helper.TFDConnection.pas',
  Comp.Generator.DataProxy in '..\..\src\Comp.Generator.DataProxy.pas',
  Comp.Generator.DataSetCode in 'Comp.Generator.DataSetCode.pas',
  App.AppInfo in 'App.AppInfo.pas',
  DataModule.Main in 'DataModule.Main.pas' {DataModule1: TDataModule},
  Form.Main in 'Form.Main.pas' {FormMain},
  Command.GenerateProxy in 'Command.GenerateProxy.pas',
  Dialog.SelectDefinition in 'Dialog.SelectDefinition.pas' {DialogSelectDefinition},
  Dialog.QueryBuilder in 'Dialog.QueryBuilder.pas' {DialogQueryBuilder};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TDataModule1, DataModule1);
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
