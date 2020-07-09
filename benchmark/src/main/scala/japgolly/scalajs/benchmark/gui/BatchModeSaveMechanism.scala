package japgolly.scalajs.benchmark.gui

import japgolly.scalajs.benchmark.vendor.{FileSaver, JSZip}
import japgolly.scalajs.react._
import scala.scalajs.js

sealed trait BatchModeSaveMechanism { self =>
  type State
  val label       : String
  val desc        : String
  val initialState: State
  val saveSuite   : (State, BatchModeSaveMechanism.SaveCmd) => CallbackTo[State]
  val saveBatch   : (State, BatchModeSaveMechanism.SaveBatchCmd) => AsyncCallback[Unit]

  final type AndState = BatchModeSaveMechanism.AndState { val value: self.type }

  final def andState(s: State): AndState =
    BatchModeSaveMechanism.AndState(this)(s)

  final def andInitialState: AndState =
    andState(initialState)
}

object BatchModeSaveMechanism {

  final case class SaveCmd(format: SuiteResultsFormat.Text,
                           args  : SuiteResultsFormat.Args[_])

  final case class SaveBatchCmd(filenameWithoutExt: String)

  case object Individually extends BatchModeSaveMechanism {
    override type State       = Unit
    override val label        = "Individually"
    override val desc         = "One file per format will be saved as soon as a BM suite completes"
    override val initialState = ()
    override val saveSuite    = (_, cmd) => cmd.format.save(cmd.args)
    override val saveBatch    = (_, _) => AsyncCallback.unit
  }

  case object CombinedZip extends BatchModeSaveMechanism {
    override type State       = Vector[(SaveCmd, js.Date)]
    override val label        = "Combined zip file"
    override val desc         = "All results will be combined into a single zip file which will be saved once all BMs have completed"
    override val initialState = Vector.empty
    override val saveSuite    = (s, cmd) => CallbackTo(s :+ ((cmd, new js.Date)))
    override val saveBatch    = (state, saveBatchCmd) => {
      val zip = new JSZip()
      val filename = s"${saveBatchCmd.filenameWithoutExt}.zip"

      // Add files to zip
      for ((cmd, cmdDate) <- state) {
        val filename = cmd.args.filename(cmd.format.fileExt)
        val content  = cmd.format.renderToText(cmd.args)
        val fileOpts = new JSZip.FileOptions {
          override val date = cmdDate
          override val unixPermissions = "644"
        }
        zip.file(filename, content, fileOpts)
      }

      for {
        blob <- AsyncCallback.fromJsPromise(zip.generateAsync(genOptions))
        _    <- AsyncCallback.delay(FileSaver.saveAs(blob, filename))
      } yield ()
    }

    private val genOptions = new JSZip.GenerationOptions {
      override val `type` = "blob"
      override val compression = "DEFLATE"
      override val compressionOptions = new JSZip.CompressionOptions {
        override val level = 9
      }
    }
  }

  val orderedValues: Vector[BatchModeSaveMechanism] =
    Vector(Individually, CombinedZip)

  implicit val reusability: Reusability[BatchModeSaveMechanism] =
    Reusability.derive

  def default = Individually

  sealed trait AndState {
    val value: BatchModeSaveMechanism
    val state: value.State

    final def saveBatch(cmd: SaveBatchCmd) =
      value.saveBatch(state, cmd)
  }

  object AndState {
    def apply(sm: BatchModeSaveMechanism)(s: sm.State): AndState { val value: sm.type } =
      new AndState {
        override val value: sm.type = sm
        override val state = s
      }

    implicit def reusability: Reusability[AndState] =
      Reusability.byRef
  }

}
