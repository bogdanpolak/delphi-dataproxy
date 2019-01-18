unit Dialog.SelectDefinition;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ActnList,
  Plus.Types;

type
  TDialogSelectDefinition = class(TForm)
    GroupBox1: TGroupBox;
    ListBox1: TListBox;
    btnSelect: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ListBox1DblClick(Sender: TObject);
  private
    actSelectConnection: TAction;
    FConnectionList: TStringArray;
    procedure actSelectConnectionExecute (Sender: TObject);
    procedure actSelectConnectionUpdate (Sender: TObject);
  public
    SelectedConnectionDef: string;
    class function Execute: String;
  end;


implementation

{$R *.dfm}

uses DataModule.Main;

class function TDialogSelectDefinition.Execute: String;
var
  dialog: TDialogSelectDefinition;
  mr: Integer;
begin
  dialog := TDialogSelectDefinition.Create(nil);
  try
    mr := dialog.ShowModal;
    if mr = mrOK then
      Result := dialog.SelectedConnectionDef
    else
      Result := ''
  finally
    dialog.Free;
  end;
end;

procedure TDialogSelectDefinition.FormCreate(Sender: TObject);
var
  DefinitionNames: TStringArray;
  s: String;
begin
  ListBox1.Clear;
  DefinitionNames := DataModule1.GetConnectionDefList;
  for s in DefinitionNames do
    ListBox1.Items.Add(s);
  FConnectionList := DataModule1.GetConnectionDefList;
  // ------------------------------------------------------------------
  // Configure action: actSelectConnection
  actSelectConnection := TAction.Create(Self);
  actSelectConnection.Caption := 'Select FireDAC connection';
  actSelectConnection.OnExecute := actSelectConnectionExecute;
  actSelectConnection.OnUpdate := actSelectConnectionUpdate;
  // ------------------------------------------------------------------
  btnSelect.Action := actSelectConnection;
end;

procedure TDialogSelectDefinition.actSelectConnectionExecute(Sender: TObject);
begin
  if ListBox1.ItemIndex >= 0 then
  begin
    SelectedConnectionDef := ListBox1.Items[ListBox1.ItemIndex];
    ModalResult := mrOK;
  end;
end;

procedure TDialogSelectDefinition.actSelectConnectionUpdate(Sender: TObject);
begin
  actSelectConnection.Enabled := (ListBox1.ItemIndex >= 0);
end;

procedure TDialogSelectDefinition.ListBox1DblClick(Sender: TObject);
begin
  actSelectConnection.Execute;
end;

end.
