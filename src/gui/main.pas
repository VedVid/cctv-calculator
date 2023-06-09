unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, StdCtrls, Menus, Spin;

type

  { TMainForm }

  TMainForm = class(TForm)
    CalculateButton2: TButton;
    CalculateButton: TButton;
    FpsGroupBox: TGroupBox;
    InstallationDataGroupBox: TGroupBox;
    AverageDistanceLabel: TLabel;
    Label5: TLabel;
    AboutTabSheet: TTabSheet;
    AboutScreenLabel1: TLabel;
    AboutScreenLabel2: TLabel;
    AboutScreenLabel3: TLabel;
    AboutScreenLabel5: TLabel;
    AboutScreenLabel4: TLabel;
    AboutScreenLabel6: TLabel;
    AboutScreenLabel7: TLabel;
    AboutScreenLabel8: TLabel;
    AboutScreenLabel9: TLabel;
    AboutScreenLabel10: TLabel;
    AboutScreenLabel11: TLabel;
    AboutScreenLabel12: TLabel;
    TransmittersDataLabel: TLabel;
    NumberOfReceiversLabel: TLabel;
    NumberOfTransmittersLabel: TLabel;
    MaxDistanceLabel: TLabel;
    NumberOfReceiversSpinEdit: TSpinEdit;
    AverageDistanceSpinEdit: TSpinEdit;
    TotalBitrateLabel2: TLabel;
    TotalCamerasLabel: TLabel;
    ModelLabel: TLabel;
    ManufacturerLabel: TLabel;
    SelectTransmitterGroupBox: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    BitratePerCameraLabel: TLabel;
    SelectModelListBox: TListBox;
    SelectManufacturerListBox: TListBox;
    NumberOfCamerasSpinEdit: TSpinEdit;
    TotalBitrateSpinEdit: TSpinEdit;
    MaxDistanceSpinEdit: TSpinEdit;
    NumberOfTransmittersSpinEdit: TSpinEdit;
    StartScreenLogo1Label: TLabel;
    StartScreenLogo2Label: TLabel;
    StartScreenTabSheet: TTabSheet;
    TransmittersTabSheet: TTabSheet;
    TotalBitrateLabel: TLabel;
    NumberOfCamerasGroupBox: TGroupBox;
    ResolutionGroupBox: TGroupBox;
    QualityGroupBox: TGroupBox;
    CompressionGroupBox: TGroupBox;
    PageControl1: TPageControl;
    QcifRadio: TRadioButton;
    Mpix6Radio: TRadioButton;
    Mpix8Radio: TRadioButton;
    Mpix12Radio: TRadioButton;
    Mpix16Radio: TRadioButton;
    LowQualityRadio: TRadioButton;
    MediumQualityRadio: TRadioButton;
    HighQualityRadio: TRadioButton;
    MjpegRadio: TRadioButton;
    Mpeg2Radio: TRadioButton;
    Mpeg4Radio: TRadioButton;
    CifRadio: TRadioButton;
    h264Radio: TRadioButton;
    h265Radio: TRadioButton;
    h265PlusRadio: TRadioButton;
    FourCifRadio: TRadioButton;
    D1Radio: TRadioButton;
    Mpix1Radio: TRadioButton;
    Mpix2Radio: TRadioButton;
    Mpix3Radio: TRadioButton;
    Mpix4Radio: TRadioButton;
    Mpix5Radio: TRadioButton;
    ImageSizeTabSheet: TTabSheet;
    NumberOfCamerasTrackBar: TTrackBar;
    FpsTrackBar: TTrackBar;
    procedure AverageDistanceSpinEditChange(Sender: TObject);
    procedure CalculateButton2Click(Sender: TObject);
    procedure CalculateButtonClick(Sender: TObject);
    procedure CifRadioEnter(Sender: TObject);
    procedure D1RadioEnter(Sender: TObject);
    procedure FourCifRadioEnter(Sender: TObject);
    procedure FpsTrackBarChange(Sender: TObject);
    procedure h264RadioExit(Sender: TObject);
    procedure h265PlusRadioExit(Sender: TObject);
    procedure h265RadioExit(Sender: TObject);
    procedure HighQualityRadioEnter(Sender: TObject);
    procedure LowQualityRadioEnter(Sender: TObject);
    procedure MaxDistanceSpinEditChange(Sender: TObject);
    procedure MediumQualityRadioEnter(Sender: TObject);
    procedure MjpegRadioExit(Sender: TObject);
    procedure Mpeg2RadioExit(Sender: TObject);
    procedure Mpeg4RadioExit(Sender: TObject);
    procedure Mpix12RadioEnter(Sender: TObject);
    procedure Mpix16RadioEnter(Sender: TObject);
    procedure Mpix1RadioEnter(Sender: TObject);
    procedure Mpix2RadioEnter(Sender: TObject);
    procedure Mpix3RadioEnter(Sender: TObject);
    procedure Mpix4RadioEnter(Sender: TObject);
    procedure Mpix5RadioEnter(Sender: TObject);
    procedure Mpix6RadioEnter(Sender: TObject);
    procedure Mpix8RadioEnter(Sender: TObject);
    procedure NumberOfCamerasSpinEditChange(Sender: TObject);
    procedure NumberOfCamerasTrackBarChange(Sender: TObject);
    procedure NumberOfReceiversSpinEditChange(Sender: TObject);
    procedure NumberOfTransmittersSpinEditChange(Sender: TObject);
    procedure QcifRadioEnter(Sender: TObject);
    procedure SelectManufacturerListBoxSelectionChange(Sender: TObject;
      User: boolean);
    procedure SelectModelListBoxSelectionChange(Sender: TObject; User: boolean);
    procedure TotalBitrateSpinEditChange(Sender: TObject);

  private

  public

  end;

var
  MainForm: TMainForm;

  Resolution: String = 'QCIF';
  Quality: String = 'Low';
  Compression: String = 'MJPEG';
  FPS: String = '1';
  NumberOfCameras: String = '1';

  CurrentManufacturer: Ansistring = '';
  CurrentModel: Ansistring = '';
  Manufacturers: Array [0..1] of Ansistring = ('CAMSAT', 'Ubiquity');
  CamsatModels: Array [0..7] of Ansistring = ('CAM-5816h',
                                              'CAM-Analog2.0',
                                              'CDS-6IP 3PoE',
                                              'CDS-6IP eco',
                                              'CDS-6IP Multi',
                                              'CDS-EasyIP eco',
                                              'CDS-EasyIP PoE',
                                              'TCO-5807h');
  NumberOfCameras2: String = '1';
  TotalBitrate2: String = '1';
  NumberOfTransmitters: String = '1';
  NumberOfReceivers: String = '1';
  MaxDistance: String = '0';
  AverageDistance: String = '0';


implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.CalculateButtonClick(Sender: TObject);
var
  flags: String = '';
  strs: TStringList;
begin
  DeleteFile('cameras_bandwidth.csv');
  flags := '--resolution ' + Resolution + ' --compression ' + Compression + ' --quality ' + Quality + ' --fps ' + FPS + ' --cameras ' + NumberOfCameras;
  SysUtils.ExecuteProcess('./calcback.exe', flags, []);
  strs := tStringList.Create;
  strs.LoadFromFile('cameras_bandwidth.csv');
  MainForm.BitratePerCameraLabel.Caption := 'Bitrate per camera: ' + strs[0].Split(',')[0] + ' Mbps';
  MainForm.TotalBitrateLabel.Caption := 'Total bitrate: ' + strs[0].Split(',')[1] + ' Mbps';
end;

procedure TMainForm.AverageDistanceSpinEditChange(Sender: TObject);
begin
  AverageDistance := IntToStr(MainForm.AverageDistanceSpinEdit.Value);
end;

procedure TMainForm.CalculateButton2Click(Sender: TObject);
var
  flags: String = '';
  strs: TStringList;
  s: String = '';
  status: String;
begin
  DeleteFile('transmitters_validation.csv');
  MainForm.Label5.Caption := '';
  flags := '--cameras ' + NumberOfCameras2 + ' --bitrate ' + TotalBitrate2 + ' --transmitters ' + NumberOfTransmitters + ' --receivers ' + NumberOfReceivers + ' --distancemax ' + MaxDistance + ' --distanceaverage ' + AverageDistance + ' --manufacturer "' + CurrentManufacturer + '" --model "' + CurrentModel + '"';
  //flags := '-m ' + NumberOfCameras2 + ' -b ' + TotalBitrate2 + ' -t ' + NumberOfTransmitters + ' -e ' + NumberOfReceivers + ' -d ' + MaxDistance + ' -i ' + AverageDistance + ' -a "' + CurrentManufacturer + '" -o "' + CurrentModel + '"';
  SysUtils.ExecuteProcess('./calcback.exe', flags, []);
  strs := TStringList.Create;
  strs.LoadFromFile('transmitters_validation.csv');
  for s in strs do
    MainForm.Label5.Caption := MainForm.Label5.Caption + LineEnding + s;
  status := Trim(MainForm.Label5.Caption);
  if status = '' then
    MainForm.Label5.Caption := 'OK';
end;


procedure TMainForm.CifRadioEnter(Sender: TObject);
begin
  Resolution := 'CIF';
end;

procedure TMainForm.QcifRadioEnter(Sender: TObject);
begin
  Resolution := 'QCIF';
end;

procedure TMainForm.SelectManufacturerListBoxSelectionChange(Sender: TObject;
  User: boolean);
var
  model: string;
begin
  CurrentManufacturer := MainForm.SelectManufacturerListBox.Items[MainForm.SelectManufacturerListBox.ItemIndex];
  MainForm.SelectModelListBox.Clear;
  if CurrentManufacturer = 'CAMSAT' then
    begin
      for model in CamsatModels do
        MainForm.SelectModelListBox.Items.Add(model);
      MainForm.SelectModelListBox.ItemIndex := 0;
    end;
end;

procedure TMainForm.SelectModelListBoxSelectionChange(Sender: TObject;
  User: boolean);
begin
  CurrentModel := MainForm.SelectModelListBox.Items[MainForm.SelectModelListBox.ItemIndex];
end;

procedure TMainForm.TotalBitrateSpinEditChange(Sender: TObject);
begin
  TotalBitrate2 := IntToStr(MainForm.TotalBitrateSpinEdit.Value);
end;

procedure TMainForm.FourCifRadioEnter(Sender: TObject);
begin
  Resolution := '4CIF';
end;

procedure TMainForm.D1RadioEnter(Sender: TObject);
begin
  Resolution := 'D1';
end;

procedure TMainForm.Mpix12RadioEnter(Sender: TObject);
begin
  Resolution := '"12 Mpix"';
end;

procedure TMainForm.Mpix16RadioEnter(Sender: TObject);
begin
  Resolution := '"16 Mpix"';
end;

procedure TMainForm.Mpix1RadioEnter(Sender: TObject);
begin
  Resolution := '"1 Mpix"';
end;

procedure TMainForm.Mpix2RadioEnter(Sender: TObject);
begin
  Resolution := '"2 Mpix"';
end;

procedure TMainForm.Mpix3RadioEnter(Sender: TObject);
begin
  Resolution := '"3 Mpix"';
end;

procedure TMainForm.Mpix4RadioEnter(Sender: TObject);
begin
  Resolution := '"4 Mpix"';
end;

procedure TMainForm.Mpix5RadioEnter(Sender: TObject);
begin
  Resolution := '"5 Mpix"';
end;

procedure TMainForm.Mpix6RadioEnter(Sender: TObject);
begin
  Resolution := '"6 Mpix"';
end;

procedure TMainForm.Mpix8RadioEnter(Sender: TObject);
begin
  Resolution := '"8 Mpix"';
end;

procedure TMainForm.NumberOfCamerasSpinEditChange(Sender: TObject);
begin
  NumberOfCameras2 := IntToStr(MainForm.NumberOfCamerasSpinEdit.Value);
end;

procedure TMainForm.LowQualityRadioEnter(Sender: TObject);
begin
  Quality := 'Low';
end;

procedure TMainForm.MaxDistanceSpinEditChange(Sender: TObject);
begin
  MaxDistance := IntToStr(MaxDistanceSpinEdit.Value);
end;

procedure TMainForm.MediumQualityRadioEnter(Sender: TObject);
begin
  Quality := 'Medium';
end;

procedure TMainForm.HighQualityRadioEnter(Sender: TObject);
begin
  Quality := 'High';
end;

procedure TMainForm.MjpegRadioExit(Sender: TObject);
begin
  Compression := 'MJPEG';
end;

procedure TMainForm.Mpeg2RadioExit(Sender: TObject);
begin
  Compression := 'MPEG-2';
end;

procedure TMainForm.Mpeg4RadioExit(Sender: TObject);
begin
  Compression := 'MPEG-4';
end;

procedure TMainForm.h264RadioExit(Sender: TObject);
begin
  Compression := 'h.264';
end;

procedure TMainForm.h265RadioExit(Sender: TObject);
begin
  Compression := 'h.265';
end;

procedure TMainForm.h265PlusRadioExit(Sender: TObject);
begin
  Compression := 'h.265+';
end;

procedure TMainForm.FpsTrackBarChange(Sender: TObject);
begin
  FPS := AnsiString(IntToStr(MainForm.FpsTrackBar.Position));
  MainForm.FpsGroupBox.Caption := 'FPS (1-30): ' + FPS;
end;

procedure TMainForm.NumberOfCamerasTrackBarChange(Sender: TObject);
begin
  NumberOfCameras := AnsiString(IntToStr(MainForm.NumberOfCamerasTrackBar.Position));
  MainForm.NumberOfCamerasGroupBox.Caption := 'Number of cameras (1-15): ' + NumberOfCameras;
end;

procedure TMainForm.NumberOfReceiversSpinEditChange(Sender: TObject);
begin
  NumberOfReceivers := IntToStr(MainForm.NumberOfReceiversSpinEdit.Value);
end;

procedure TMainForm.NumberOfTransmittersSpinEditChange(Sender: TObject);
begin
  NumberOfTransmitters := IntToStr(MainForm.NumberOfTransmittersSpinEdit.Value);
end;

end.

