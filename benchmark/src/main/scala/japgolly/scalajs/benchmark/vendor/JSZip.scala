package japgolly.scalajs.benchmark.vendor

import org.scalajs.dom.Blob
import scala.annotation.nowarn
import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal
import scala.scalajs.js.typedarray.{ArrayBuffer, Uint8Array}
import scala.scalajs.js.{Date, Promise, |}

@JSGlobal("JSZip")
@js.native
@nowarn
final class JSZip extends js.Object {
  import JSZip._

  def file(name   : String,
           data   : String | ArrayBuffer | Uint8Array | Blob,
           options: FileOptions = js.native): this.type = js.native

  def folder(name: String): JSZip = js.native

  def generateAsync(options: GenerationOptions): Promise[Blob] = js.native

}

@nowarn
object JSZip {

  trait FileOptions extends js.Object {

    /** set to true if the data is base64 encoded. For example image data from a canvas element. Plain text and HTML do not need this option. */
    val base64: js.UndefOr[Boolean] = js.undefined

    /** set to true if the data should be treated as raw content, false if this is a text. If base64 is used, this defaults to true, if the data is not a string, this will be set to true */
    val binary: js.UndefOr[Boolean] = js.undefined

    /** The comment for this file */
    val comment: js.UndefOr[String] = js.undefined

    /** If set, specifies compression method to use for this specific file. If not, the default file compression will be used */
    val compression: js.UndefOr[String] = js.undefined

    val compressionOptions: js.UndefOr[CompressionOptions] = js.undefined

    /** Set to true if folders in the file path should be automatically created, otherwise there will only be virtual folders that represent the path to the file */
    val createFolders: js.UndefOr[Boolean] = js.undefined

    /** the last modification date */
    val date: js.UndefOr[Date] = js.undefined

    /** Set to true if this is a directory and content should be ignored */
    val dir: js.UndefOr[Boolean] = js.undefined

    /** The DOS permissions of the file, if any */
    val dosPermissions: js.UndefOr[Byte] = js.undefined

    /** Set to true if (and only if) the input is a “binary string” and has already been prepared with a 0xFF mask. */
    val optimizedBinaryString: js.UndefOr[Boolean] = js.undefined

    /** The UNIX permissions of the file, if any. Also accepts a string representing the octal value: “644”, “755”, etc. */
    val unixPermissions: js.UndefOr[Short | String] = js.undefined
  }

  trait CompressionOptions extends js.Object {
    val level: js.UndefOr[Int] = js.undefined
  }

  trait GenerationOptions extends js.Object {

    /** The zip format has no flag or field to give the encoding of this field and JSZip will use UTF-8.
      * With non ASCII characters you might get encoding issues if the file archiver doesn’t use UTF-8 (or the given
      * encoding) to decode the comment.
      */
    val comment: js.UndefOr[String] = js.undefined

    /** Available compression methods are STORE (no compression) and DEFLATE.
      *
      * Note : if the entry is already compressed (coming from a compressed zip file), calling generateAsync() with a
      * different compression level won’t update the entry. The reason is simple : JSZip doesn’t know how compressed the
      * content was and how to match the compression level with the implementation we use.
      */
    val compression: js.UndefOr[String] = js.undefined

    val compressionOptions: js.UndefOr[CompressionOptions] = js.undefined

    val encodeFileName: js.UndefOr[js.Function1[String, String]] = js.undefined

    val mimeType: js.UndefOr[String] = js.undefined

    val platform: js.UndefOr[String] = js.undefined

    val streamFiles: js.UndefOr[Boolean] = js.undefined

    val `type`: "blob"
  }
}
