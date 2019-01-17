unit Dialog.SelectDefinition;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf,
  FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys,
  FireDAC.VCLUI.Wait, Data.DB, FireDAC.Comp.Client;

type
  TDialogSelectDefinition = class(TForm)
    GroupBox1: TGroupBox;
    ListBox1: TListBox;
    btnSelect: TButton;
    FDConnection1: TFDConnection;
    procedure btnSelectClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
  public
    SelectedConnectionDef: string;
    class function Execute: String;
  end;


implementation

{$R *.dfm}

uses DataModule.Main;

procedure TDialogSelectDefinition.btnSelectClick(Sender: TObject);
begin
  if ListBox1.ItemIndex>=0 then
  begin
    SelectedConnectionDef := ListBox1.Items[ListBox1.ItemIndex];
    ModalResult := mrOK;
  end;
end;

procedure TDialogSelectDefinition.FormCreate(Sender: TObject);
var
  i: Integer;
  cdef: IFDStanConnectionDef;
  DefinitionNames: TStringArray;
  s: String;
begin
  ListBox1.Clear;
  DefinitionNames := DataModule1.GetConnectionDefList;
  for s in DefinitionNames do
    ListBox1.Items.Add(s);
  btnSelect.Enabled := (FDManager.ConnectionDefs.Count>0);
end;

{ TDialogSelectDefinition }

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

end.
