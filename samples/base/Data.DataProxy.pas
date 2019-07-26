unit Data.DataProxy;

interface

uses
  System.Classes,
  System.SysUtils,
  Data.DB;

type
  TGenericDataSetProxy = class(TComponent)
  private
  protected
    FDataSet: TDataSet;
    procedure ConnectFields; virtual; abstract;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // -------------
    procedure ConnectWithDataSet(aDataSet: TDataSet);
    // -------------
    procedure Append; inline;
    procedure Cancel; inline;
    procedure Close; inline;
    function ControlsDisabled: Boolean;
    function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream;
    procedure Delete; inline;
    procedure DisableControls;
    procedure Edit; inline;
    procedure EnableControls;
    procedure First; inline;
    procedure Insert; inline;
    procedure InsertRecord(const Values: array of const);
    function IsEmpty: Boolean; inline;
    procedure Last; inline;
    function Locate(const KeyFields: string; const KeyValues: Variant;
      Options: TLocateOptions): Boolean;
    function Lookup(const KeyFields: string; const KeyValues: Variant;
      const ResultFields: string): Variant;
    procedure Next; inline;
    procedure Open;
    procedure Post; inline;
    procedure Prior; inline;
    procedure Refresh; inline;
    function UpdateStatus: TUpdateStatus;
  end;

  TDatasetProxy = class(TGenericDataSetProxy)
    procedure ForEach(OnElem: TProc<TDatasetProxy>);
  end;

implementation

{ TGenericDataObject }

uses Helper.TDataSet;

procedure TGenericDataSetProxy.Append;
begin
  FDataSet.Append;
end;

procedure TGenericDataSetProxy.Cancel;
begin
  FDataSet.Cancel;
end;

procedure TGenericDataSetProxy.Close;
begin
  FDataSet.Close;
end;

procedure TGenericDataSetProxy.ConnectWithDataSet(aDataSet: TDataSet);
begin
  FDataSet := aDataSet;
end;

function TGenericDataSetProxy.ControlsDisabled: Boolean;
begin
  Result := FDataSet.ControlsDisabled;
end;

constructor TGenericDataSetProxy.Create(AOwner: TComponent);
begin
  inherited;
  // TODO: Implement
end;

function TGenericDataSetProxy.CreateBlobStream(Field: TField;
  Mode: TBlobStreamMode): TStream;
begin
  Result := FDataSet.CreateBlobStream(Field, Mode);
end;

procedure TGenericDataSetProxy.Delete;
begin
  FDataSet.Delete;
end;

destructor TGenericDataSetProxy.Destroy;
begin

  inherited;
end;

procedure TGenericDataSetProxy.DisableControls;
begin
  FDataSet.DisableControls;
end;

procedure TGenericDataSetProxy.Edit;
begin
  FDataSet.Edit;
end;

procedure TGenericDataSetProxy.EnableControls;
begin
  FDataSet.EnableControls;
end;

procedure TGenericDataSetProxy.First;
begin
  FDataSet.First;
end;

procedure TGenericDataSetProxy.Insert;
begin
  FDataSet.Insert;
end;

procedure TGenericDataSetProxy.InsertRecord(const Values: array of const);
begin
  FDataSet.InsertRecord(Values);
end;

function TGenericDataSetProxy.IsEmpty: Boolean;
begin
  Result := FDataSet.IsEmpty;
end;

procedure TGenericDataSetProxy.Last;
begin
  FDataSet.Last;
end;

function TGenericDataSetProxy.Locate(const KeyFields: string;
  const KeyValues: Variant; Options: TLocateOptions): Boolean;
begin
  Result := FDataSet.Locate(KeyFields, KeyValues, Options);
end;

function TGenericDataSetProxy.Lookup(const KeyFields: string;
  const KeyValues: Variant; const ResultFields: string): Variant;
begin
  Result := FDataSet.Lookup(KeyFields, KeyValues, ResultFields);
end;

procedure TGenericDataSetProxy.Next;
begin
  FDataSet.Next;
end;

procedure TGenericDataSetProxy.Open;
begin
  FDataSet.Open;
  ConnectFields;
end;

procedure TGenericDataSetProxy.Post;
begin
  FDataSet.Post;
end;

procedure TGenericDataSetProxy.Prior;
begin
  FDataSet.Prior;
end;

procedure TGenericDataSetProxy.Refresh;
begin
  FDataSet.Refresh;
end;

function TGenericDataSetProxy.UpdateStatus: TUpdateStatus;
begin
  Result := FDataSet.UpdateStatus;
end;

{ TItarableDataObject }

procedure TDatasetProxy.ForEach(OnElem: TProc<TDatasetProxy>);
begin
  // TODO: Wyci�gnij kod z helper-a (reu�ywalno�� vs wydajno��)
  FDataSet.WhileNotEof(
    procedure()
    begin
      OnElem (self);
    end);
end;

end.
