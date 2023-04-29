unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    CalculateButton: TButton;
    FpsGroupBox: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    BitratePerCameraLabel: TLabel;
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
    procedure NumberOfCamerasTrackBarChange(Sender: TObject);
    procedure QcifRadioEnter(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;
  Resolution: String = 'QCIF';
  Quality: String = 'Low';
  Compression: String = 'MJPEG';
  FPS: String = '1';
  NumberOfCameras: String = '1';

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.CalculateButtonClick(Sender: TObject);
var
  flags: String = '';
  strs: TStringList;
begin
  flags := '-r ' + Resolution + ' -c ' + Compression + ' -q ' + Quality + ' -f ' + FPS + ' -m ' + NumberOfCameras;
  SysUtils.ExecuteProcess('./cameras_bandwidth_calculator.exe', flags, []);
  strs := tStringList.Create;
  strs.LoadFromFile('finished_cameras_bandwidth_calculator.csv');
  Form1.BitratePerCameraLabel.Caption := 'Bitrate per camera: ' + strs[1].Split(',')[0] + ' Mbps';
  Form1.TotalBitrateLabel.Caption := 'Total bitrate: ' + strs[1].Split(',')[2] + ' Mbps';
end;


procedure TForm1.CifRadioEnter(Sender: TObject);
begin
  Resolution := 'CIF';
end;

procedure TForm1.QcifRadioEnter(Sender: TObject);
begin
  Resolution := 'QCIF';
end;

procedure TForm1.FourCifRadioEnter(Sender: TObject);
begin
  Resolution := '4CIF';
end;

procedure TForm1.D1RadioEnter(Sender: TObject);
begin
  Resolution := 'D1';
end;

procedure TForm1.Mpix12RadioEnter(Sender: TObject);
begin
  Resolution := '"12 Mpix"';
end;

procedure TForm1.Mpix16RadioEnter(Sender: TObject);
begin
  Resolution := '"16 Mpix"';
end;

procedure TForm1.Mpix1RadioEnter(Sender: TObject);
begin
  Resolution := '"1 Mpix"';
end;

procedure TForm1.Mpix2RadioEnter(Sender: TObject);
begin
  Resolution := '"2 Mpix"';
end;

procedure TForm1.Mpix3RadioEnter(Sender: TObject);
begin
  Resolution := '"3 Mpix"';
end;

procedure TForm1.Mpix4RadioEnter(Sender: TObject);
begin
  Resolution := '"4 Mpix"';
end;

procedure TForm1.Mpix5RadioEnter(Sender: TObject);
begin
  Resolution := '"5 Mpix"';
end;

procedure TForm1.Mpix6RadioEnter(Sender: TObject);
begin
  Resolution := '"6 Mpix"';
end;

procedure TForm1.Mpix8RadioEnter(Sender: TObject);
begin
  Resolution := '"8 Mpix"';
end;

procedure TForm1.LowQualityRadioEnter(Sender: TObject);
begin
  Quality := 'Low';
end;

procedure TForm1.MediumQualityRadioEnter(Sender: TObject);
begin
  Quality := 'Medium';
end;

procedure TForm1.HighQualityRadioEnter(Sender: TObject);
begin
  Quality := 'High';
end;

procedure TForm1.MjpegRadioExit(Sender: TObject);
begin
  Compression := 'MJPEG';
end;

procedure TForm1.Mpeg2RadioExit(Sender: TObject);
begin
  Compression := 'MPEG-2';
end;

procedure TForm1.Mpeg4RadioExit(Sender: TObject);
begin
  Compression := 'MPEG-4';
end;

procedure TForm1.h264RadioExit(Sender: TObject);
begin
  Compression := 'h.264';
end;

procedure TForm1.h265RadioExit(Sender: TObject);
begin
  Compression := 'h.265';
end;

procedure TForm1.h265PlusRadioExit(Sender: TObject);
begin
  Compression := 'h.265+';
end;

procedure TForm1.FpsTrackBarChange(Sender: TObject);
begin
  FPS := AnsiString(IntToStr(Form1.FpsTrackBar.Position));
  Form1.FpsGroupBox.Caption := 'FPS (1-30): ' + FPS;
end;

procedure TForm1.NumberOfCamerasTrackBarChange(Sender: TObject);
begin
  NumberOfCameras := AnsiString(IntToStr(Form1.NumberOfCamerasTrackBar.Position));
  Form1.NumberOfCamerasGroupBox.Caption := 'Number of cameras (1-15): ' + NumberOfCameras;
end;

end.

