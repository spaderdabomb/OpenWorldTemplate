// Amplify Impostors
// Copyright (c) Amplify Creations, Lda <info@amplify.pt>

using System;
using UnityEngine;
using UnityEngine.Rendering;
using UnityEditor;
using UnityEditor.PackageManager.Requests;
using System.Collections.Generic;
using System.IO;
using System.Reflection;
using System.Text.RegularExpressions;

namespace AmplifyImpostors
{
	public enum AIImportFlags
	{
		None = 0,
		URP = 1 << 0,
		HDRP = 1 << 1,
		Both = URP | HDRP
	}

	public enum AISRPBaseline
	{
		AI_SRP_INVALID = 0,
		AI_SRP_10 = 100000,
		AI_SRP_11 = 110000,
		AI_SRP_12 = 120000,
		AI_SRP_13 = 130000,
		AI_SRP_14 = 140000,
		AI_SRP_15 = 150000,
		AI_SRP_16 = 160000,
		AI_SRP_17 = 170000
	}

	public class AISRPPackageDesc
	{
		public AISRPBaseline baseline = AISRPBaseline.AI_SRP_INVALID;
		public string guidURP = string.Empty;
		public string guidHDRP = string.Empty;

		public AISRPPackageDesc( AISRPBaseline baseline, string guidURP, string guidHDRP )
		{
			this.baseline = baseline;
			this.guidURP = guidURP;
			this.guidHDRP = guidHDRP;
		}
	}

	[Serializable]
	[InitializeOnLoad]
	public static class AIPackageManagerHelper
	{
		private static string URPPackageId = "com.unity.render-pipelines.universal";
		private static string HDRPPackageId = "com.unity.render-pipelines.high-definition";

		private static string NewVersionDetectedFormat = "[AmplifyImpostors] A new {0} version {1} was detected and compatible shaders are being imported.\nPlease hit the Update button on your ASE canvas to recompile your shader under the newest version.";
		private static string PackageBaseFormat = "AI_PkgBase_{0}_{1}";
		private static string PackageCRCFormat = "AI_PkgCRC_{0}_{1}";

		private static string URPBakingGUID = "491949b665cd59d43bc93c611092dcae";
		private static string URPBakeTemplateGUID = "6ee191abcace33c46a5dd52068b074e0";
		private static string URPTemplateGUID = "ad3e6cbf440fc8843823405e7e862e1a";
		private static string URPOctahedronGUID = "83dd8de9a5c14874884f9012def4fdcc";
		private static string URPSphericalGUID = "da79d698f4bf0164e910ad798d07efdf";

		private static string HDRPBakingGUID = "fab6af2c092e89d4b8da1cdd41c05392";
		private static string HDRPBakeTemplateGUID = "5b7fbe5f8e132bd40b11a10c99044f79";
		private static string HDRPTemplateGUID = "87e83e0e165df8c42a86552d2fc010f1";
		private static string HDRPOctahedronGUID = "56236dc63ad9b7949b63a27f0ad180b3";
		private static string HDRPSphericalGUID = "175c951fec709c44fa2f26b8ab78b8dd";

		private const string ImpostorsGCincGUID = "806d6cc0f22ee994f8cd901b6718f08d";

		private static Dictionary<int, AISRPPackageDesc> m_srpPackageSupport = new Dictionary<int, AISRPPackageDesc>()
		{
			{ ( int )AISRPBaseline.AI_SRP_10, new AISRPPackageDesc( AISRPBaseline.AI_SRP_10, "06e710174fd46404391092ae9bc5e849", "d7ac8a02737091445aa206cd5bbc7101" ) },
			{ ( int )AISRPBaseline.AI_SRP_11, new AISRPPackageDesc( AISRPBaseline.AI_SRP_11, "06e710174fd46404391092ae9bc5e849", "d7ac8a02737091445aa206cd5bbc7101" ) },
			{ ( int )AISRPBaseline.AI_SRP_12, new AISRPPackageDesc( AISRPBaseline.AI_SRP_12, "a51904d3fea17d942a8935be648c3f0d", "741ca0d5f350e034e84999facc1de789" ) },
			{ ( int )AISRPBaseline.AI_SRP_13, new AISRPPackageDesc( AISRPBaseline.AI_SRP_13, "a51904d3fea17d942a8935be648c3f0d", "741ca0d5f350e034e84999facc1de789" ) },
			{ ( int )AISRPBaseline.AI_SRP_14, new AISRPPackageDesc( AISRPBaseline.AI_SRP_14, "ade3eaef5ceb09e42ade0a2d51d48465", "417c33caa5dee86498451657f089dfba" ) },
			{ ( int )AISRPBaseline.AI_SRP_15, new AISRPPackageDesc( AISRPBaseline.AI_SRP_15, "3ba162bb5fa749244af39891d40a737e", "50089e2e3ccdc9a4185eea86c35460c4" ) },
			{ ( int )AISRPBaseline.AI_SRP_16, new AISRPPackageDesc( AISRPBaseline.AI_SRP_16, "5d2a70fd39a1c484486c3719d8fca5d9", "0488f1df97596c544b1640896e525f9c" ) },
			{ ( int )AISRPBaseline.AI_SRP_17, new AISRPPackageDesc( AISRPBaseline.AI_SRP_17, "d63ae6d0e8b3f2049a20f6ad644d825d", "e341a9468c915f64e928c68b621a78ea" ) },
		};

		public static bool Supports( AISRPBaseline baseline ) { return m_srpPackageSupport.ContainsKey( ( int )baseline ); }

		private static ListRequest m_packageListRequest = null;
		private static UnityEditor.PackageManager.PackageInfo m_urpPackageInfo;
		private static UnityEditor.PackageManager.PackageInfo m_hdrpPackageInfo;

		private static bool m_lateImport = false;
		private static string m_latePackageToImport;
		private static bool m_requireUpdateList = false;
		private static AIImportFlags m_importingPackage = AIImportFlags.None;

		private static AISRPBaseline m_currentURPBaseline = AISRPBaseline.AI_SRP_INVALID;
		private static AISRPBaseline m_currentHDRPBaseline = AISRPBaseline.AI_SRP_INVALID;

		public static AISRPBaseline CurrentURPBaseline { get { return m_currentURPBaseline; } }
		public static AISRPBaseline CurrentHDRPBaseline { get { return m_currentHDRPBaseline; } }

		private static int m_packageURPVersion = 0; // @diogo: starts as missing
		private static int m_packageHDRPVersion = 0;

		public static int PackageURPBaseline { get { return m_packageURPVersion; } }
		public static int PackageHDRPBaseline { get { return m_packageHDRPVersion; } }

		private static string m_projectName = null;
		private static string ProjectName
		{
			get
			{
				if ( string.IsNullOrEmpty( m_projectName ) )
				{
					string[] s = Application.dataPath.Split( '/' );
					m_projectName = s[ s.Length - 2 ];
				}
				return m_projectName;
			}
		}

		static AIPackageManagerHelper()
		{
			RequestInfo();
		}

		static void WaitForPackageListBeforeUpdating()
		{
			if ( m_packageListRequest.IsCompleted )
			{
				Update();
				EditorApplication.update -= WaitForPackageListBeforeUpdating;
			}
		}

		public static void RequestInfo()
		{
			if ( !m_requireUpdateList && m_importingPackage == AIImportFlags.None )
			{
				m_requireUpdateList = true;
				m_packageListRequest = UnityEditor.PackageManager.Client.List( true );
				EditorApplication.update += WaitForPackageListBeforeUpdating;
			}
		}

		static void FailedPackageImport( string packageName, string errorMessage )
		{
			FinishImporter();
		}

		static void CancelledPackageImport( string packageName )
		{
			FinishImporter();
		}

		static void CompletedPackageImport( string packageName )
		{
			FinishImporter();
		}

		public static void CheckLatePackageImport()
		{
			if ( !Application.isPlaying && m_lateImport && !string.IsNullOrEmpty( m_latePackageToImport ) )
			{
				m_lateImport = false;
				StartImporting( m_latePackageToImport );
				m_latePackageToImport = string.Empty;
			}
		}

		public static void StartImporting( string packagePath )
		{
			if ( !Preferences.GlobalAutoSRP )
			{
				m_importingPackage = AIImportFlags.None;
				return;
			}

			if ( Application.isPlaying )
			{
				if ( !m_lateImport )
				{
					m_lateImport = true;
					m_latePackageToImport = packagePath;
					Debug.LogWarning( "Amplify Impostors requires the \"" + packagePath + "\" package to be installed in order to continue. Please exit Play mode to proceed." );
				}
				return;
			}

			AssetDatabase.importPackageCancelled += CancelledPackageImport;
			AssetDatabase.importPackageCompleted += CompletedPackageImport;
			AssetDatabase.importPackageFailed += FailedPackageImport;
			AssetDatabase.ImportPackage( packagePath, false );
		}

		public static void FinishImporter()
		{
			m_importingPackage = AIImportFlags.None;
			AssetDatabase.importPackageCancelled -= CancelledPackageImport;
			AssetDatabase.importPackageCompleted -= CompletedPackageImport;
			AssetDatabase.importPackageFailed -= FailedPackageImport;
		}

		private static readonly string SemVerPattern = @"^(0|[1-9]\d*)\.(0|[1-9]\d*)\.(0|[1-9]\d*)(?:-((?:0|[1-9]\d*|\d*[a-zA-Z-][0-9a-zA-Z-]*)(?:\.(?:0|[1-9]\d*|\d*[a-zA-Z-][0-9a-zA-Z-]*))*))?(?:\+([0-9a-zA-Z-]+(?:\.[0-9a-zA-Z-]+)*))?$";

		public static int PackageVersionStringToCode( string version, out int major, out int minor, out int patch )
		{
			MatchCollection matches = Regex.Matches( version, SemVerPattern, RegexOptions.Multiline );

			bool validMatch = ( matches.Count > 0 && matches[ 0 ].Groups.Count >= 4 );
			major = validMatch ? int.Parse( matches[ 0 ].Groups[ 1 ].Value ) : 99;
			minor = validMatch ? int.Parse( matches[ 0 ].Groups[ 2 ].Value ) : 99;
			patch = validMatch ? int.Parse( matches[ 0 ].Groups[ 3 ].Value ) : 99;

			int versionCode;
			versionCode = major * 10000;
			versionCode += minor * 100;
			versionCode += patch;
			return versionCode;
		}

		private static int PackageVersionElementsToCode( int major, int minor, int patch )
		{
			return major * 10000 + minor * 100 + patch;
		}

		private static void CheckPackageImport( AIImportFlags flag, AISRPBaseline baseline, string guid, string version )
		{
			Debug.Assert( flag == AIImportFlags.HDRP || flag == AIImportFlags.URP );

			string path = AssetDatabase.GUIDToAssetPath( guid );

			if ( !string.IsNullOrEmpty( path ) && File.Exists( path ) )
			{
				uint currentCRC = CRC32( File.ReadAllBytes( path ) );

				string srpName = flag.ToString();
				string packageBaseKey = string.Format( PackageBaseFormat, srpName, ProjectName );
				string packageCRCKey = string.Format( PackageCRCFormat, srpName, ProjectName );

				AISRPBaseline savedBaseline = ( AISRPBaseline )EditorPrefs.GetInt( packageBaseKey );
				uint savedCRC = ( uint )EditorPrefs.GetInt( packageCRCKey, 0 );

				bool foundNewVersion = ( savedBaseline != baseline ) || ( savedCRC != currentCRC );

				EditorPrefs.SetInt( packageBaseKey, ( int )baseline );
				EditorPrefs.SetInt( packageCRCKey, ( int )currentCRC );

				string testPath0 = string.Empty;
				string testPath1 = string.Empty;
				string testPath2 = string.Empty;
				string testPath3 = string.Empty;
				string testPath4 = string.Empty;

				switch ( flag )
				{
					case AIImportFlags.URP:
					{
						testPath0 = AssetDatabase.GUIDToAssetPath( URPBakingGUID );
						testPath1 = AssetDatabase.GUIDToAssetPath( URPBakeTemplateGUID );
						testPath2 = AssetDatabase.GUIDToAssetPath( URPTemplateGUID );
						testPath3 = AssetDatabase.GUIDToAssetPath( URPOctahedronGUID );
						testPath4 = AssetDatabase.GUIDToAssetPath( URPSphericalGUID );
						break;
					}
					case AIImportFlags.HDRP:
					{
						testPath0 = AssetDatabase.GUIDToAssetPath( HDRPBakingGUID );
						testPath1 = AssetDatabase.GUIDToAssetPath( HDRPBakeTemplateGUID );
						testPath2 = AssetDatabase.GUIDToAssetPath( HDRPTemplateGUID );
						testPath3 = AssetDatabase.GUIDToAssetPath( HDRPOctahedronGUID );
						testPath4 = AssetDatabase.GUIDToAssetPath( HDRPSphericalGUID );
						break;
					}
				}

				if ( foundNewVersion || !File.Exists( testPath0 ) || !File.Exists( testPath1 ) || !File.Exists( testPath2 ) || !File.Exists( testPath3 ) || !File.Exists( testPath4 ) )
				{
					if ( foundNewVersion )
					{
						Debug.Log( string.Format( NewVersionDetectedFormat, srpName, version ) );
					}
					m_importingPackage |= flag;
					StartImporting( path );
				}
			}
		}

		public static void Update()
		{
			CheckLatePackageImport();

			if ( m_requireUpdateList && m_importingPackage == AIImportFlags.None )
			{
				if ( m_packageListRequest != null && m_packageListRequest.IsCompleted )
				{
					m_requireUpdateList = false;
					foreach ( UnityEditor.PackageManager.PackageInfo pi in m_packageListRequest.Result )
					{
						int version = PackageVersionStringToCode( pi.version, out int major, out int minor, out int patch );
						int baseline = PackageVersionElementsToCode( major, 0, 0 );
						AISRPPackageDesc match;

						if ( pi.name.Equals( URPPackageId ) && m_srpPackageSupport.TryGetValue( baseline, out match ) )
						{
							// Universal Rendering Pipeline
							m_currentURPBaseline = match.baseline;
							m_packageURPVersion = version;
							m_urpPackageInfo = pi;

							CheckPackageImport( AIImportFlags.URP, match.baseline, match.guidURP, pi.version );
						}
						else if ( pi.name.Equals( HDRPPackageId ) && m_srpPackageSupport.TryGetValue( baseline, out match ) )
						{
							// High-Definition Rendering Pipeline
							m_currentHDRPBaseline = match.baseline;
							m_packageHDRPVersion = version;
							m_hdrpPackageInfo = pi;

							CheckPackageImport( AIImportFlags.HDRP, match.baseline, match.guidHDRP, pi.version );
						}
					}

					// Make sure AmplifyImpostors.cginc is updated
					ApplySRP();
				}
			}
		}

		private static void ApplySRP()
		{
			string impostorCGincPath = AssetDatabase.GUIDToAssetPath( ImpostorsGCincGUID );
			if ( string.IsNullOrEmpty( impostorCGincPath ) )
				return;

			string cginc = string.Empty;
			if ( !string.IsNullOrEmpty( impostorCGincPath ) && File.Exists( impostorCGincPath ) )
			{
				cginc = File.ReadAllText( impostorCGincPath );
			}

			bool saveAndRefresh = false;

			Match cgincMatch = Regex.Match( cginc, @"#define AI_HDRP_VERSION (\d*)", RegexOptions.Multiline );
			if ( cgincMatch.Success )
			{
				string cgincSRPversion = cgincMatch.Groups[ 1 ].Value;
				if ( cgincSRPversion != ( ( int )m_packageHDRPVersion ).ToString() )
				{
					cginc = cginc.Replace( cgincMatch.Groups[ 0 ].Value, "#define AI_HDRP_VERSION " + ( ( int )m_packageHDRPVersion ).ToString() );
					saveAndRefresh = true;
				}
			}

			cgincMatch = Regex.Match( cginc, @"#define AI_URP_VERSION (\d*)", RegexOptions.Multiline );
			if ( cgincMatch.Success )
			{
				string cgincSRPversion = cgincMatch.Groups[ 1 ].Value;
				if ( cgincSRPversion != ( ( int )m_packageURPVersion ).ToString() )
				{
					cginc = cginc.Replace( cgincMatch.Groups[ 0 ].Value, "#define AI_URP_VERSION " + ( ( int )m_packageURPVersion ).ToString() );
					saveAndRefresh = true;
				}
			}

			if ( saveAndRefresh )
			{
				File.WriteAllText( impostorCGincPath, cginc );
			}
		}

		// Polynomial: 0xedb88320
		static readonly uint[] crc32_tab = {
			0x00000000, 0x77073096, 0xee0e612c, 0x990951ba, 0x076dc419, 0x706af48f,
			0xe963a535, 0x9e6495a3, 0x0edb8832, 0x79dcb8a4, 0xe0d5e91e, 0x97d2d988,
			0x09b64c2b, 0x7eb17cbd, 0xe7b82d07, 0x90bf1d91, 0x1db71064, 0x6ab020f2,
			0xf3b97148, 0x84be41de, 0x1adad47d, 0x6ddde4eb, 0xf4d4b551, 0x83d385c7,
			0x136c9856, 0x646ba8c0, 0xfd62f97a, 0x8a65c9ec, 0x14015c4f, 0x63066cd9,
			0xfa0f3d63, 0x8d080df5, 0x3b6e20c8, 0x4c69105e, 0xd56041e4, 0xa2677172,
			0x3c03e4d1, 0x4b04d447, 0xd20d85fd, 0xa50ab56b, 0x35b5a8fa, 0x42b2986c,
			0xdbbbc9d6, 0xacbcf940, 0x32d86ce3, 0x45df5c75, 0xdcd60dcf, 0xabd13d59,
			0x26d930ac, 0x51de003a, 0xc8d75180, 0xbfd06116, 0x21b4f4b5, 0x56b3c423,
			0xcfba9599, 0xb8bda50f, 0x2802b89e, 0x5f058808, 0xc60cd9b2, 0xb10be924,
			0x2f6f7c87, 0x58684c11, 0xc1611dab, 0xb6662d3d, 0x76dc4190, 0x01db7106,
			0x98d220bc, 0xefd5102a, 0x71b18589, 0x06b6b51f, 0x9fbfe4a5, 0xe8b8d433,
			0x7807c9a2, 0x0f00f934, 0x9609a88e, 0xe10e9818, 0x7f6a0dbb, 0x086d3d2d,
			0x91646c97, 0xe6635c01, 0x6b6b51f4, 0x1c6c6162, 0x856530d8, 0xf262004e,
			0x6c0695ed, 0x1b01a57b, 0x8208f4c1, 0xf50fc457, 0x65b0d9c6, 0x12b7e950,
			0x8bbeb8ea, 0xfcb9887c, 0x62dd1ddf, 0x15da2d49, 0x8cd37cf3, 0xfbd44c65,
			0x4db26158, 0x3ab551ce, 0xa3bc0074, 0xd4bb30e2, 0x4adfa541, 0x3dd895d7,
			0xa4d1c46d, 0xd3d6f4fb, 0x4369e96a, 0x346ed9fc, 0xad678846, 0xda60b8d0,
			0x44042d73, 0x33031de5, 0xaa0a4c5f, 0xdd0d7cc9, 0x5005713c, 0x270241aa,
			0xbe0b1010, 0xc90c2086, 0x5768b525, 0x206f85b3, 0xb966d409, 0xce61e49f,
			0x5edef90e, 0x29d9c998, 0xb0d09822, 0xc7d7a8b4, 0x59b33d17, 0x2eb40d81,
			0xb7bd5c3b, 0xc0ba6cad, 0xedb88320, 0x9abfb3b6, 0x03b6e20c, 0x74b1d29a,
			0xead54739, 0x9dd277af, 0x04db2615, 0x73dc1683, 0xe3630b12, 0x94643b84,
			0x0d6d6a3e, 0x7a6a5aa8, 0xe40ecf0b, 0x9309ff9d, 0x0a00ae27, 0x7d079eb1,
			0xf00f9344, 0x8708a3d2, 0x1e01f268, 0x6906c2fe, 0xf762575d, 0x806567cb,
			0x196c3671, 0x6e6b06e7, 0xfed41b76, 0x89d32be0, 0x10da7a5a, 0x67dd4acc,
			0xf9b9df6f, 0x8ebeeff9, 0x17b7be43, 0x60b08ed5, 0xd6d6a3e8, 0xa1d1937e,
			0x38d8c2c4, 0x4fdff252, 0xd1bb67f1, 0xa6bc5767, 0x3fb506dd, 0x48b2364b,
			0xd80d2bda, 0xaf0a1b4c, 0x36034af6, 0x41047a60, 0xdf60efc3, 0xa867df55,
			0x316e8eef, 0x4669be79, 0xcb61b38c, 0xbc66831a, 0x256fd2a0, 0x5268e236,
			0xcc0c7795, 0xbb0b4703, 0x220216b9, 0x5505262f, 0xc5ba3bbe, 0xb2bd0b28,
			0x2bb45a92, 0x5cb36a04, 0xc2d7ffa7, 0xb5d0cf31, 0x2cd99e8b, 0x5bdeae1d,
			0x9b64c2b0, 0xec63f226, 0x756aa39c, 0x026d930a, 0x9c0906a9, 0xeb0e363f,
			0x72076785, 0x05005713, 0x95bf4a82, 0xe2b87a14, 0x7bb12bae, 0x0cb61b38,
			0x92d28e9b, 0xe5d5be0d, 0x7cdcefb7, 0x0bdbdf21, 0x86d3d2d4, 0xf1d4e242,
			0x68ddb3f8, 0x1fda836e, 0x81be16cd, 0xf6b9265b, 0x6fb077e1, 0x18b74777,
			0x88085ae6, 0xff0f6a70, 0x66063bca, 0x11010b5c, 0x8f659eff, 0xf862ae69,
			0x616bffd3, 0x166ccf45, 0xa00ae278, 0xd70dd2ee, 0x4e048354, 0x3903b3c2,
			0xa7672661, 0xd06016f7, 0x4969474d, 0x3e6e77db, 0xaed16a4a, 0xd9d65adc,
			0x40df0b66, 0x37d83bf0, 0xa9bcae53, 0xdebb9ec5, 0x47b2cf7f, 0x30b5ffe9,
			0xbdbdf21c, 0xcabac28a, 0x53b39330, 0x24b4a3a6, 0xbad03605, 0xcdd70693,
			0x54de5729, 0x23d967bf, 0xb3667a2e, 0xc4614ab8, 0x5d681b02, 0x2a6f2b94,
			0xb40bbe37, 0xc30c8ea1, 0x5a05df1b, 0x2d02ef8d
		};

		private static uint CRC32( byte[] buf, uint crc = 0 )
		{
			uint i = 0;
			uint size = ( uint )buf.Length;
			crc = crc ^ 0xFFFFFFFF;
			while ( size-- > 0 )
			{
				crc = crc32_tab[ ( crc ^ buf[ i++ ] ) & 0xFF ] ^ ( crc >> 8 );
			}
			return crc ^ 0xFFFFFFFF;
		}
	}

	public sealed class TemplatePostProcessor : AssetPostprocessor
	{
		static void OnPostprocessAllAssets( string[] importedAssets, string[] deletedAssets, string[] movedAssets, string[] movedFromAssetPaths )
		{
			AIPackageManagerHelper.RequestInfo();
		}
	}
}