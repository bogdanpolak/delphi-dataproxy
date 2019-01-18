program ProxyGenerator;

uses
  Vcl.Forms,
  Form.Main in 'Form.Main.pas' {FormMain},
  Dialog.SelectDefinition in 'Dialog.SelectDefinition.pas' {DialogSelectDefinition},
  DataModule.Main in 'DataModule.Main.pas' {DataModule1: TDataModule},
  Helper.TApplication in 'Helper.TApplication.pas',
  Plus.ProxyGenerator in 'Plus.ProxyGenerator.pas',
  App.AppInfo in 'App.AppInfo.pas',
  Helper.TDBGrid in 'Helper.TDBGrid.pas',
  Plus.Types in 'Plus.Types.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TDataModule1, DataModule1);
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
