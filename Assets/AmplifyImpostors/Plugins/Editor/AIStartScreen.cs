// Amplify Impostors
// Copyright (c) Amplify Creations, Lda <info@amplify.pt>

using UnityEngine;
using UnityEditor;
using System;
using UnityEngine.Networking;
using System.Collections;
using System.Collections.Generic;

namespace AmplifyImpostors
{
	public enum TemplateSRPType
	{
		BiRP,
		HDRP,
		URP
	}

	public class AIStartScreen : EditorWindow
	{
		[MenuItem( "Window/Amplify Impostors/Start Screen", false, 1999 )]
		public static void Init()
		{
			AIStartScreen window = ( AIStartScreen )GetWindow( typeof( AIStartScreen ), true, "Amplify Impostors Start Screen" );
			window.minSize = new Vector2( 650, 500 );
			window.maxSize = new Vector2( 650, 500 );
			window.Show();
		}

		private static readonly string ChangeLogGUID = "967f64c31d8dde244a5e92f47deea593";
		private static readonly string BuiltInGUID = "ec7e2bd19b32d4c42948cb7cce07b40c";

		private static readonly string IconGUID = "1070aab9cfe961c409d48e3bec7f7ab0";

		public static readonly string ChangelogURL = "https://amplify.pt/Banner/AIchangelog.json";

		private static readonly string ManualURL = "https://wiki.amplify.pt/index.php?title=Unity_Products:Amplify_Impostors/Manual";

		private static readonly string DiscordURL = "https://discordapp.com/invite/EdrVAP5";
		private static readonly string ForumURL = "https://forum.unity.com/threads/amplify-impostors-next-generation-billboards.539844/";

		private static readonly string SiteURL = "http://amplify.pt/download/";
		private static readonly string StoreURL = "https://assetstore.unity.com/packages/tools/utilities/amplify-impostors-119877";

		private static readonly GUIContent SamplesTitle = new GUIContent( "Samples", "Import samples according to you project rendering pipeline" );
		private static readonly GUIContent ResourcesTitle = new GUIContent( "Learning Resources", "Check the online wiki for various topics about how to use AI with node examples and explanations" );
		private static readonly GUIContent CommunityTitle = new GUIContent( "Community", "Need help? Reach us through our discord server or the official support Unity forum" );
		private static readonly GUIContent UpdateTitle = new GUIContent( "Latest Update", "Check the lastest additions, improvements and bug fixes done to AI" );
		private static readonly GUIContent AITitle = new GUIContent( "Amplify Impostors", "Are you using the latest version? Now you know" );

		private const string OnlineVersionWarning = "Please enable \"Allow downloads over HTTP*\" in Player Settings to access latest version information via Start Screen.";

		Vector2 m_scrollPosition = Vector2.zero;
		Preferences.ShowOption m_startup = Preferences.ShowOption.Never;

		[NonSerialized]
		Texture packageIcon = null;
		[NonSerialized]
		Texture textIcon = null;
		[NonSerialized]
		Texture webIcon = null;

		GUIContent HDRPbutton = null;
		GUIContent URPbutton = null;
		GUIContent BuiltInbutton = null;

		GUIContent Manualbutton = null;

		GUIContent DiscordButton = null;
		GUIContent ForumButton = null;

		GUIContent AIIcon = null;
		RenderTexture rt;

		[NonSerialized]
		GUIStyle m_buttonStyle = null;
		[NonSerialized]
		GUIStyle m_buttonLeftStyle = null;
		[NonSerialized]
		GUIStyle m_buttonRightStyle = null;
		[NonSerialized]
		GUIStyle m_minibuttonStyle = null;
		[NonSerialized]
		GUIStyle m_labelStyle = null;
		[NonSerialized]
		GUIStyle m_linkStyle = null;

		private ChangeLogInfo m_changeLog;
		private bool m_infoDownloaded = false;
		private string m_newVersion = string.Empty;

		private static Dictionary<int, AISRPPackageDesc> m_srpSamplePackages = new Dictionary<int, AISRPPackageDesc>()
		{
			{ ( int )AISRPBaseline.AI_SRP_10, new AISRPPackageDesc( AISRPBaseline.AI_SRP_10, "17dc0f553d1635444b54871e620d94e9", "df2b682938298cd4497689766b792e60" ) },
			{ ( int )AISRPBaseline.AI_SRP_11, new AISRPPackageDesc( AISRPBaseline.AI_SRP_11, "17dc0f553d1635444b54871e620d94e9", "df2b682938298cd4497689766b792e60" ) },
			{ ( int )AISRPBaseline.AI_SRP_12, new AISRPPackageDesc( AISRPBaseline.AI_SRP_12, "b96ac023f1ef6c144891ecea1fa57ae8", "9d8cb26fa0bbd5743910e10e476c1b34" ) },
			{ ( int )AISRPBaseline.AI_SRP_13, new AISRPPackageDesc( AISRPBaseline.AI_SRP_13, "b96ac023f1ef6c144891ecea1fa57ae8", "9d8cb26fa0bbd5743910e10e476c1b34" ) },
			{ ( int )AISRPBaseline.AI_SRP_14, new AISRPPackageDesc( AISRPBaseline.AI_SRP_14, "c297d913695beab48aafeed7e786c21e", "4592c91874062c644b0606bd98342356" ) },
			{ ( int )AISRPBaseline.AI_SRP_15, new AISRPPackageDesc( AISRPBaseline.AI_SRP_15, "49ea3764a534e4347848722ad53b9bf1", "f61a93a0b64d3dd499926d4ad5816cb1" ) },
			{ ( int )AISRPBaseline.AI_SRP_16, new AISRPPackageDesc( AISRPBaseline.AI_SRP_16, "349761993cf8d6a41970c379725073d4", "0fd95d7c6a1a1864eb85fef533f5e5df" ) },
			{ ( int )AISRPBaseline.AI_SRP_17, new AISRPPackageDesc( AISRPBaseline.AI_SRP_17, "e2267cb0adc217c4eac2799ec1a5efbc", "3325eb58a80f8e842b7cec8916b0e236" ) },
		};

		private void OnEnable()
		{
			rt = new RenderTexture( 16, 16, 0 );
			rt.Create();

			m_startup = ( Preferences.ShowOption )EditorPrefs.GetInt( Preferences.PrefGlobalStartUp, 0 );

			if ( textIcon == null )
			{
				Texture icon = EditorGUIUtility.IconContent( "TextAsset Icon" ).image;
				var cache = RenderTexture.active;
				RenderTexture.active = rt;
				Graphics.Blit( icon, rt );
				RenderTexture.active = cache;
				textIcon = rt;

				Manualbutton = new GUIContent( " Manual", textIcon );
			}

			if ( packageIcon == null )
			{
				packageIcon = EditorGUIUtility.IconContent( "BuildSettings.Editor.Small" ).image;
				HDRPbutton = new GUIContent( " HDRP Samples", packageIcon );
				URPbutton = new GUIContent( " URP Samples", packageIcon );
				BuiltInbutton = new GUIContent( " Built-In Samples", packageIcon );
			}

			if ( webIcon == null )
			{
				webIcon = EditorGUIUtility.IconContent( "BuildSettings.Web.Small" ).image;
				DiscordButton = new GUIContent( " Discord", webIcon );
				ForumButton = new GUIContent( " Unity Forum", webIcon );
			}

			if ( m_changeLog == null )
			{
				var changelog = AssetDatabase.LoadAssetAtPath<TextAsset>( AssetDatabase.GUIDToAssetPath( ChangeLogGUID ) );
				string lastUpdate = string.Empty;
				if ( changelog != null )
				{
					int oldestReleaseIndex = changelog.text.LastIndexOf( string.Format( "v{0}.{1}.{2}", VersionInfo.Major, VersionInfo.Minor, VersionInfo.Release ) );

					lastUpdate = changelog.text.Substring( 0, changelog.text.IndexOf( "\nv", oldestReleaseIndex + 25 ) );// + "\n...";
					lastUpdate = lastUpdate.Replace( "* ", "\u2022 " );
				}
				m_changeLog = new ChangeLogInfo( VersionInfo.FullNumber, lastUpdate );
			}

			if ( AIIcon == null )
			{
				AIIcon = new GUIContent( AssetDatabase.LoadAssetAtPath<Texture2D>( AssetDatabase.GUIDToAssetPath( IconGUID ) ) );
			}
		}

		private void OnDisable()
		{
			if ( rt != null )
			{
				rt.Release();
				DestroyImmediate( rt );
			}
		}

		public void OnGUI()
		{
			if ( !m_infoDownloaded )
			{
				m_infoDownloaded = true;

				StartBackgroundTask( StartRequest( ChangelogURL, () =>
				{
					if ( string.IsNullOrEmpty( www.error ) )
					{
						var temp = ChangeLogInfo.CreateFromJSON( www.downloadHandler.text );
						if ( temp != null && temp.Version >= m_changeLog.Version )
						{
							m_changeLog = temp;
						}

						int version = m_changeLog.Version;
						int major = version / 10000;
						int minor = version / 1000 - major * 10;
						int release = version / 100 - ( version / 1000 ) * 10;
						int revision = version - ( version / 100 ) * 100;

						m_newVersion = major + "." + minor + "." + release + ( revision > 0 ? "." + revision : "" );

						Repaint();
					}
				} ) );
			}

			if ( m_buttonStyle == null )
			{
				m_buttonStyle = new GUIStyle( GUI.skin.button );
				m_buttonStyle.alignment = TextAnchor.MiddleLeft;
			}

			if ( m_buttonLeftStyle == null )
			{
				m_buttonLeftStyle = new GUIStyle( "ButtonLeft" );
				m_buttonLeftStyle.alignment = TextAnchor.MiddleLeft;
				m_buttonLeftStyle.margin = m_buttonStyle.margin;
				m_buttonLeftStyle.margin.right = 0;
			}

			if ( m_buttonRightStyle == null )
			{
				m_buttonRightStyle = new GUIStyle( "ButtonRight" );
				m_buttonRightStyle.alignment = TextAnchor.MiddleLeft;
				m_buttonRightStyle.margin = m_buttonStyle.margin;
				m_buttonRightStyle.margin.left = 0;
			}

			if ( m_minibuttonStyle == null )
			{
				m_minibuttonStyle = new GUIStyle( "MiniButton" );
				m_minibuttonStyle.alignment = TextAnchor.MiddleLeft;
				m_minibuttonStyle.margin = m_buttonStyle.margin;
				m_minibuttonStyle.margin.left = 20;
				m_minibuttonStyle.normal.textColor = m_buttonStyle.normal.textColor;
				m_minibuttonStyle.hover.textColor = m_buttonStyle.hover.textColor;
			}

			if ( m_labelStyle == null )
			{
				m_labelStyle = new GUIStyle( "BoldLabel" );
				m_labelStyle.margin = new RectOffset( 4, 4, 4, 4 );
				m_labelStyle.padding = new RectOffset( 2, 2, 2, 2 );
				m_labelStyle.fontSize = 13;
			}

			if ( m_linkStyle == null )
			{
				var inv = AssetDatabase.LoadAssetAtPath<Texture2D>( AssetDatabase.GUIDToAssetPath( "a91a70303ba684645a7a87a0ddec0eb7" ) ); // find a better solution for transparent buttons
				m_linkStyle = new GUIStyle();
				m_linkStyle.normal.textColor = new Color( 0.2980392f, 0.4901961f, 1f );
				m_linkStyle.hover.textColor = Color.white;
				m_linkStyle.active.textColor = Color.grey;
				m_linkStyle.margin.top = 3;
				m_linkStyle.margin.bottom = 2;
				m_linkStyle.hover.background = inv;
				m_linkStyle.active.background = inv;
			}

			EditorGUILayout.BeginHorizontal( GUIStyle.none, GUILayout.ExpandWidth( true ) );
			{
				// left column
				EditorGUILayout.BeginVertical( GUILayout.Width( 175 ) );
				{
					GUILayout.Label( SamplesTitle, m_labelStyle );
					EditorGUILayout.BeginHorizontal();
					if ( GUILayout.Button( HDRPbutton, m_buttonLeftStyle ) )
						ImportSample( HDRPbutton.text, TemplateSRPType.HDRP );

					EditorGUILayout.EndHorizontal();

					EditorGUILayout.BeginHorizontal();
					if ( GUILayout.Button( URPbutton, m_buttonLeftStyle ) )
						ImportSample( URPbutton.text, TemplateSRPType.URP );

					EditorGUILayout.EndHorizontal();
					if ( GUILayout.Button( BuiltInbutton, m_buttonStyle ) )
						ImportSample( BuiltInbutton.text, TemplateSRPType.BiRP );

					GUILayout.Space( 10 );

					GUILayout.Label( ResourcesTitle, m_labelStyle );
					if ( GUILayout.Button( Manualbutton, m_buttonStyle ) )
						Application.OpenURL( ManualURL );
				}
				EditorGUILayout.EndVertical();

				// right column
				EditorGUILayout.BeginVertical( GUILayout.Width( 650 - 175 - 9 ), GUILayout.ExpandHeight( true ) );
				{
					GUILayout.Label( CommunityTitle, m_labelStyle );
					EditorGUILayout.BeginHorizontal( GUILayout.ExpandWidth( true ) );
					{
						if ( GUILayout.Button( DiscordButton, GUILayout.ExpandWidth( true ) ) )
						{
							Application.OpenURL( DiscordURL );
						}
						if ( GUILayout.Button( ForumButton, GUILayout.ExpandWidth( true ) ) )
						{
							Application.OpenURL( ForumURL );
						}
					}
					EditorGUILayout.EndHorizontal();
					GUILayout.Label( UpdateTitle, m_labelStyle );
					m_scrollPosition = GUILayout.BeginScrollView( m_scrollPosition, "ProgressBarBack", GUILayout.ExpandHeight( true ), GUILayout.ExpandWidth( true ) );
					GUILayout.Label( m_changeLog.LastUpdate, "WordWrappedMiniLabel", GUILayout.ExpandHeight( true ) );
					GUILayout.EndScrollView();

					EditorGUILayout.BeginHorizontal( GUILayout.ExpandWidth( true ) );
					{
						EditorGUILayout.BeginVertical();
						GUILayout.Label( AITitle, m_labelStyle );

						GUILayout.Label( "Installed Version: " + VersionInfo.StaticToString() );

						if ( m_changeLog.Version > VersionInfo.FullNumber )
						{
							var cache = GUI.color;
							GUI.color = Color.red;
							GUILayout.Label( "New version available: " + m_newVersion, "BoldLabel" );
							GUI.color = cache;
						}
						else
						{
							var cache = GUI.color;
							GUI.color = Color.green;
							GUILayout.Label( "You are using the latest version", "BoldLabel" );
							GUI.color = cache;
						}

						EditorGUILayout.BeginHorizontal();
						GUILayout.Label( "Download links:" );
						if ( GUILayout.Button( "Amplify", m_linkStyle ) )
							Application.OpenURL( SiteURL );
						GUILayout.Label( "-" );
						if ( GUILayout.Button( "Asset Store", m_linkStyle ) )
							Application.OpenURL( StoreURL );
						EditorGUILayout.EndHorizontal();
						GUILayout.Space( 7 );
						EditorGUILayout.EndVertical();

						GUILayout.FlexibleSpace();
						EditorGUILayout.BeginVertical();
						GUILayout.Space( 7 );
						GUILayout.Label( AIIcon );
						EditorGUILayout.EndVertical();
					}
					EditorGUILayout.EndHorizontal();
				}
				EditorGUILayout.EndVertical();
			}
			EditorGUILayout.EndHorizontal();


			EditorGUILayout.BeginHorizontal( "ProjectBrowserBottomBarBg", GUILayout.ExpandWidth( true ), GUILayout.Height( 22 ) );
			{
				GUILayout.FlexibleSpace();
				EditorGUI.BeginChangeCheck();
				var cache = EditorGUIUtility.labelWidth;
				EditorGUIUtility.labelWidth = 100;
				m_startup = ( Preferences.ShowOption )EditorGUILayout.EnumPopup( "Show At Startup", m_startup, GUILayout.Width( 220 ) );
				EditorGUIUtility.labelWidth = cache;
				if ( EditorGUI.EndChangeCheck() )
				{
					EditorPrefs.SetInt( Preferences.PrefGlobalStartUp, ( int )m_startup );
				}
			}
			EditorGUILayout.EndHorizontal();
		}

		void ImportSample( string pipeline, TemplateSRPType srpType )
		{
			if ( EditorUtility.DisplayDialog( "Import Sample", "This will import the samples for" + pipeline.Replace( " Samples", "" ) + ", please make sure the pipeline is properly installed and/or selected before importing the samples.\n\nContinue?", "Yes", "No" ) )
			{
				switch ( srpType )
				{
					case TemplateSRPType.BiRP:
					{
						AssetDatabase.ImportPackage( AssetDatabase.GUIDToAssetPath( BuiltInGUID ), false );
						break;
					}
					case TemplateSRPType.URP:
					{
						if ( m_srpSamplePackages.TryGetValue( ( int )AIPackageManagerHelper.CurrentURPBaseline, out AISRPPackageDesc desc ) )
						{
							string path = AssetDatabase.GUIDToAssetPath( desc.guidURP );
							if ( !string.IsNullOrEmpty( path ) )
							{
								AssetDatabase.ImportPackage( path, false );
							}
						}
						break;
					}
					case TemplateSRPType.HDRP:
					{
						if ( m_srpSamplePackages.TryGetValue( ( int )AIPackageManagerHelper.CurrentHDRPBaseline, out AISRPPackageDesc desc ) )
						{
							string path = AssetDatabase.GUIDToAssetPath( desc.guidHDRP );
							if ( !string.IsNullOrEmpty( path ) )
							{
								AssetDatabase.ImportPackage( path, false );
							}
						}
						break;
					}
					default:
					{
						// no action
						break;
					}

				}
			}
		}

		UnityWebRequest www;

		IEnumerator StartRequest( string url, Action success = null )
		{
			using ( www = UnityWebRequest.Get( url ) )
			{
				yield return www.SendWebRequest();

				while ( www.isDone == false )
					yield return null;

				if ( success != null )
					success();
			}
		}

		public static void StartBackgroundTask( IEnumerator update, Action end = null )
		{
			EditorApplication.CallbackFunction closureCallback = null;

			closureCallback = () =>
			{
				try
				{
					if ( update.MoveNext() == false )
					{
						if ( end != null )
							end();
						EditorApplication.update -= closureCallback;
					}
				}
				catch ( Exception ex )
				{
					if ( end != null )
						end();
					Debug.LogException( ex );
					EditorApplication.update -= closureCallback;
				}
			};

			EditorApplication.update += closureCallback;
		}
	}

	[Serializable]
	internal class ChangeLogInfo
	{
		public int Version;
		public string LastUpdate;

		public static ChangeLogInfo CreateFromJSON( string jsonString )
		{
			return JsonUtility.FromJson<ChangeLogInfo>( jsonString );
		}

		public ChangeLogInfo( int version, string lastUpdate )
		{
			Version = version;
			LastUpdate = lastUpdate;
		}
	}

	[InitializeOnLoad]
	public static class ShowStartScreen
	{
		static ShowStartScreen()
		{
			EditorApplication.update += Update;
		}

		static UnityWebRequest www;

		static IEnumerator StartRequest( string url, Action success = null )
		{
			using ( www = UnityWebRequest.Get( url ) )
			{
				yield return www.SendWebRequest();

				while ( www.isDone == false )
					yield return null;

				if ( success != null )
					success();
			}
		}

		static void Update()
		{
			EditorApplication.update -= Update;

			if ( !EditorApplication.isPlayingOrWillChangePlaymode )
			{
				Preferences.ShowOption show = Preferences.ShowOption.Never;
				if ( !EditorPrefs.HasKey( Preferences.PrefGlobalStartUp ) )
				{
					show = Preferences.ShowOption.Always;
					EditorPrefs.SetInt( Preferences.PrefGlobalStartUp, 0 );
				}
				else
				{
					if ( Time.realtimeSinceStartup < 10 )
					{
						show = ( Preferences.ShowOption )EditorPrefs.GetInt( Preferences.PrefGlobalStartUp, 0 );
						// check version here
						if ( show == Preferences.ShowOption.OnNewVersion )
						{
							AIStartScreen.StartBackgroundTask( StartRequest( AIStartScreen.ChangelogURL, () =>
							{
								var changeLog = ChangeLogInfo.CreateFromJSON( www.downloadHandler.text );
								if ( changeLog != null )
								{
									if ( changeLog.Version > VersionInfo.FullNumber )
									{
										AIStartScreen.Init();
									}
								}
							} ) );
						}
					}
				}

				if ( show == Preferences.ShowOption.Always )
				{
					AIStartScreen.Init();
				}
			}
		}
	}
}