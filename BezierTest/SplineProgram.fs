open System
open System.Windows
open System.Windows.Controls
open System.Windows.Data
open System.Windows.Shapes
open Microsoft.Win32
open Microsoft.FSharp.Math

type VisualizationSettings = {
    algo : float;
    num_curves : float;
    p1 : vector;
    p2 : vector;
    p3 : vector;
    p4 : vector;
}
with
    override this.ToString() =
        (sprintf ("Num curves: %f
                   \r\nControl point (p2): %f,%f
                   \r\nControl point (p3): %f,%f")
                    this.num_curves this.p2.[0] this.p2.[1] this.p3.[0] this.p3.[1])
    static member Construct = { new VisualizationSettings
                                with num_curves = 6.
                                and p1 = vector [3.; 0.]
                                and p2 = vector [6.; 7.]
                                and p3 = vector [12.; 6.]
                                and p4 = vector [15.; -1.]
                                and algo = 0.
    }
end

/// Contains functionality for visualizing a set of vectors
type RenderMatrix() =
    do () // Empty constructor

    /// Renders the given matrix data to a canvas which is returned
    static member renderScene (canvas:Canvas) (data:VisualizationSettings) width height =
        canvas.Children.Clear()

        /// Get TextBlock describing the input values
        let getDescription (data:VisualizationSettings) = 
            new TextBlock(Text=data.ToString(), Foreground=Media.SolidColorBrush(Media.Colors.DarkCyan))

        let graphBrush = Media.SolidColorBrush(Media.Colors.Turquoise)
        let rectBrush = Media.SolidColorBrush(Media.Colors.DarkSalmon)
        let blackBrush = Media.SolidColorBrush(Media.Colors.Black)

        // Defines the settings for displaying stuff on screen
        let bottomLevel = height+60.
        let leftPadding = 70.
        let screenScale = 25.

        let addLineToScreen (v1:vector) (v2:vector) brush thickness =
            canvas.Children.Add(new Line(X1 = leftPadding+v1.[0]*screenScale,
                                            Y1 = bottomLevel-v1.[1]*screenScale,
                                            X2 = leftPadding+v2.[0]*screenScale,
                                            Y2 = bottomLevel-v2.[1]*screenScale,
                                            StrokeThickness = thickness, Stroke=brush)) |> ignore

        let drawPoints p1 p2 p3 p4 =
            addLineToScreen p1 p2 rectBrush 0.8
            addLineToScreen p2 p3 rectBrush 0.8
            addLineToScreen p3 p4 rectBrush 0.8

        /// Render the tree on given canvas
        let renderPoints (previous:vector) (current:vector) = 
            addLineToScreen previous current blackBrush 1.
            current

        for i in 1 .. int data.num_curves do
            let ydiff = vector [0.; float i]
            let p1, p2, p3, p4 = (data.p1 + ydiff), (data.p2 + ydiff),
                                 (data.p3 + ydiff), (data.p4 + ydiff)
            drawPoints p1 p2 p3 p4
            let points =
                if int data.algo = 0 then
                    CubicBezier.cubic_bezier p1 p2 p3 p4 i
                else
                    CubicBezier.hermite p1 p2 p3 p4 i
            points |> List.fold renderPoints points.Head |> ignore

        canvas.Children.Insert(0, getDescription data) |> ignore

type BezierWindow() as this =
    inherit Window()
    let backgroundBrush = Media.SolidColorBrush(Media.Colors.BlanchedAlmond)
    let canvas = new Canvas(Background=backgroundBrush, Width=this.Width, Height=this.Height)
    let canvasRenderer = RenderMatrix.renderScene canvas

    /// Define controls with labels, min, initial, max values and action for value change
    let controlsAndActions = [(0., 0., 1., "Algorithm", (fun next settings -> {settings with algo = next}))
                              (2., 6., 40., "Number of curves", (fun next settings -> {settings with num_curves = next}))
                              (-20., 6., 20., "p2 X", (fun next settings -> {settings with p2 = vector [next; settings.p2.[1]]}))
                              (-20., 10., 20., "p2 Y", (fun next settings -> {settings with p2 = vector [settings.p2.[0]; next]}))
                              (-20., 12., 20., "p3 X", (fun next settings -> {settings with p3 = vector [next; settings.p3.[1]]}))
                              (-20., 9., 20., "p3 Y", (fun next settings -> {settings with p3 = vector [settings.p3.[0]; next]}))]

    /// Create controls on given panel, add defined change action and merge all observables
    let createAndGetControlEvents (panel:Controls.Panel) =
        let createControlAndRegisterEvent (min, initial, max, label, action) =
            panel.Children.Add(new Label(Content=label)) |> ignore
            let slider = new Slider(Minimum=min, Maximum=max, Value=initial)
            panel.Children.Add(slider) |> ignore
            slider.ValueChanged |> Observable.map (fun rea -> action rea.NewValue)
        controlsAndActions
          |> List.map (createControlAndRegisterEvent)
          |> List.reduce Observable.merge

    /// Re-render the screen with changed data
    let renderScene (mainPanel:Controls.Panel) data =
        mainPanel.Children.RemoveAt(1)
        canvasRenderer data (this.Width-70.) this.Height
        mainPanel.Children.Add(canvas) |> ignore

    let initControlEventsAndAddListener leftPanel mainPanel resetButtonEvent =
        let controlEvents = createAndGetControlEvents leftPanel
        Observable.merge controlEvents resetButtonEvent
            |> Observable.scan (fun a b -> b a) (VisualizationSettings.Construct)
            |> Observable.add (renderScene mainPanel)

    let recreateLeftPanel createResetButton =
        let mainPanel = this.Content :?> StackPanel
        if mainPanel.Children.Count > 1 then
            mainPanel.Children.RemoveAt(0)
        let leftPanel = new StackPanel(Orientation = Orientation.Vertical)
        initControlEventsAndAddListener leftPanel mainPanel (createResetButton leftPanel)
        mainPanel.Children.Insert(0, leftPanel) |> ignore

    let rec createResetButton (leftPanel:Controls.Panel) =
        let btn = new Button(Content="Reset")
        leftPanel.Children.Add(btn) |> ignore
        // Just ignore the old values and return the initial setting
        btn.Click |> Observable.map(fun rea data ->
            recreateLeftPanel createResetButton
            VisualizationSettings.Construct)

    do
        let mainPanel = new StackPanel(Orientation = Orientation.Horizontal, VerticalAlignment = VerticalAlignment.Top)
        mainPanel.Children.Add(new TextBlock(Text="\r\n\r\n\r\n«« Start by modifying the values on the left")) |> ignore
        this.Content <- mainPanel
        recreateLeftPanel createResetButton

[<System.STAThread>]
do
    (new Application()).Run(BezierWindow(Width=600., Height=700.,
                             Background=Media.SolidColorBrush(Media.Colors.White),
                             Title="Bezier Curve tester")) |> ignore
