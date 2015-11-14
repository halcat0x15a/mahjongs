package org.halcat.mahjongs

import scala.collection.JavaConverters._

import android.app.Activity
import android.graphics.ImageFormat
import android.hardware.Camera
import android.os.Bundle
import android.util.Log
import android.view.{View, Surface, SurfaceHolder}

import org.apache.http.client.methods.HttpPost
import org.apache.http.entity.ByteArrayEntity
import org.apache.http.impl.client.DefaultHttpClient

import TypedResource._

class MainActivity extends Activity { self =>

  override def onCreate(savedInstanceState: Bundle) = {
    super.onCreate(savedInstanceState)
    setContentView(R.layout.main)
    var camera: Camera = null
    val surfaceView = this.findView(TR.surfaceView)
    surfaceView.getHolder.addCallback(new SurfaceHolder.Callback {
      def surfaceCreated(holder: SurfaceHolder) = {
        camera = Camera.open()
        camera.setPreviewDisplay(holder)
      }
      def surfaceDestroyed(holder: SurfaceHolder) = {
        camera.release()
        camera = null
      }
      def surfaceChanged(holder: SurfaceHolder, format: Int, width: Int, height: Int) = {
        val parameters = camera.getParameters
        if (parameters.getPreviewSize.height < parameters.getPreviewSize.width && width < height) {
          camera.setDisplayOrientation(90)
        }
        parameters.setPictureFormat(ImageFormat.JPEG)
        camera.setParameters(parameters)
        camera.startPreview()
      }
    })
    val client = new DefaultHttpClient
    surfaceView.setOnClickListener(new View.OnClickListener {
      def onClick(view: View) {
        if (camera != null) {
          camera.autoFocus(new Camera.AutoFocusCallback {
            def onAutoFocus(success: Boolean, camera: Camera) = {
              camera.takePicture(null, null, new Camera.PictureCallback {
                def onPictureTaken(data: Array[Byte], camera: Camera) = {
                  new Thread(new Runnable {
                    def run = {
                      val post = new HttpPost("http://133.20.164.83:8080")
                      post.setEntity(new ByteArrayEntity(data))
                      client.execute(post)
                    }
                  }).start()
                }
              })
            }
          })
        }
      }
    })
  }

}
