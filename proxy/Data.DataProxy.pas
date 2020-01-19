{ * ------------------------------------------------------------------------
  * ♥
  * ♥  Delphi DataSetProxy component - wrapper for Delphi TDataSet
  * ♥
  * Home: https://github.com/bogdanpolak/delphi-dataproxy
  *
  * Classes:
  *   1. TGenericDataSetProxy - base class for the wrapper
  *   2. TDatasetProxy - inherited from TGenericDataSetProxy adding ForEach
  *   3. TDataProxyFactory - TDasetProxy and derived clasess factory
  *  ----------------------------------------------------------------------- * }
unit Data.DataProxy;

interface

uses
  System.Classes,
  System.SysUtils,
  Data.DB;

type
  TDataSetProxy = class(TComponent)
  private const
    Version = '0.9';
  protected
    FDataSet: TDataSet;
    procedure ConnectFields; virtual; abstract;
    procedure SetDataSet(aDataSet: TDataSet);
  public
    function WithDataSet(aDataSet: TDataSet): TDataSetProxy;
    function Open: TDataSetProxy;
    procedure ForEach(OnElem: TProc);
    // ----------------
    // TDataSet wrapped methods:
    procedure Append;
    procedure AppendRecord(const Values: array of const);
    procedure BindToDataSource(DataSource: TDataSource);
    procedure Cancel;
    function ConstructDataSource(AOwner: TComponent): TDataSource;
    procedure Close;
    function ControlsDisabled: Boolean;
    function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream;
    procedure Delete;
    procedure DisableControls;
    procedure Edit;
    procedure EnableControls;
    procedure First;
    procedure Insert;
    procedure InsertRecord(const Values: array of const);
    function IsEmpty: Boolean;
    procedure Last;
    function Eof: Boolean;
    function Locate(const KeyFields: string; const KeyValues: Variant;
      Options: TLocateOptions): Boolean;
    function Lookup(const KeyFields: string; const KeyValues: Variant;
      const ResultFields: string): Variant;
    procedure Next;
    procedure Post;
    procedure Prior;
    procedure Refresh;
    function UpdateStatus: TUpdateStatus;
  end;

implementation

// * --------------------------------------------------------------------
// * TGenericDataSetProxy
// * --------------------------------------------------------------------

procedure TDataSetProxy.Append;
begin
  FDataSet.Append;
end;

procedure TDataSetProxy.AppendRecord(const Values: array of const);
begin
  FDataSet.AppendRecord(Values);
end;

procedure TDataSetProxy.BindToDataSource(DataSource: TDataSource);
begin
  DataSource.DataSet := FDataSet;
end;

procedure TDataSetProxy.Cancel;
begin
  FDataSet.Cancel;
end;

procedure TDataSetProxy.Close;
begin
  FDataSet.Close;
end;

function TDataSetProxy.ConstructDataSource(AOwner: TComponent)
  : TDataSource;
begin
  Result := TDataSource.Create(AOwner);
  Result.DataSet := FDataSet;
end;

function TDataSetProxy.ControlsDisabled: Boolean;
begin
  Result := FDataSet.ControlsDisabled;
end;

function TDataSetProxy.CreateBlobStream(Field: TField;
  Mode: TBlobStreamMode): TStream;
begin
  Result := FDataSet.CreateBlobStream(Field, Mode);
end;

procedure TDataSetProxy.Delete;
begin
  FDataSet.Delete;
end;

procedure TDataSetProxy.DisableControls;
begin
  FDataSet.DisableControls;
end;

procedure TDataSetProxy.Edit;
begin
  FDataSet.Edit;
end;

procedure TDataSetProxy.EnableControls;
begin
  FDataSet.EnableControls;
end;

function TDataSetProxy.Eof: Boolean;
begin
  Result := FDataSet.Eof;
end;

procedure TDataSetProxy.First;
begin
  FDataSet.First;
end;

procedure TDataSetProxy.Insert;
begin
  FDataSet.Insert;
end;

procedure TDataSetProxy.InsertRecord(const Values: array of const);
begin
  FDataSet.InsertRecord(Values);
end;

function TDataSetProxy.IsEmpty: Boolean;
begin
  Result := FDataSet.IsEmpty;
end;

procedure TDataSetProxy.Last;
begin
  FDataSet.Last;
end;

function TDataSetProxy.Locate(const KeyFields: string;
  const KeyValues: Variant; Options: TLocateOptions): Boolean;
begin
  Result := FDataSet.Locate(KeyFields, KeyValues, Options);
end;

function TDataSetProxy.Lookup(const KeyFields: string;
  const KeyValues: Variant; const ResultFields: string): Variant;
begin
  Result := FDataSet.Lookup(KeyFields, KeyValues, ResultFields);
end;

procedure TDataSetProxy.Next;
begin
  FDataSet.Next;
end;

procedure TDataSetProxy.Post;
begin
  FDataSet.Post;
end;

procedure TDataSetProxy.Prior;
begin
  FDataSet.Prior;
end;

procedure TDataSetProxy.Refresh;
begin
  FDataSet.Refresh;
end;

function TDataSetProxy.UpdateStatus: TUpdateStatus;
begin
  Result := FDataSet.UpdateStatus;
end;


// * --------------------------------------------------------------------
// * Extra methods (dedicated for Proxy)
// * --------------------------------------------------------------------

procedure TDataSetProxy.SetDataSet(aDataSet: TDataSet);
begin
  fDataSet := aDataset;
  if fDataSet.Active then
    ConnectFields;
end;

function TDataSetProxy.WithDataSet(aDataSet: TDataSet): TDataSetProxy;
begin
  SetDataSet (aDataSet);
  Result := Self;
end;

function TDataSetProxy.Open: TDataSetProxy;
begin
  fDataSet.Open;
  ConnectFields;
  Result := Self;
end;

procedure TDatasetProxy.ForEach(OnElem: TProc);
var
  Bookmark: TBookmark;
begin
  self.DisableControls;
  try
    Bookmark := FDataSet.GetBookmark;
    try
      self.First;
      while not FDataSet.Eof do
      begin
        OnElem();
        FDataSet.Next;
      end;
    finally
      if FDataSet.BookmarkValid(Bookmark) then
        FDataSet.GotoBookmark(Bookmark);
      FDataSet.FreeBookmark(Bookmark);
    end;
  finally
    FDataSet.EnableControls;
  end;
end;

end.
