// Amplify Impostors
// Copyright (c) Amplify Creations, Lda <info@amplify.pt>

using System.Collections;
using System.Collections.Generic;
using System.IO;
using UnityEngine;
#if UNITY_EDITOR
using UnityEditor;
#endif
namespace AmplifyImpostors
{
	public static class ImpostorBakingTools
	{
#if UNITY_EDITOR
		public static string OpenFolderForImpostor( this AmplifyImpostor instance )
		{
			string oneLevelUp = Application.dataPath + "/../";
			string directory = Path.GetFullPath( oneLevelUp ).Replace( "\\", "/" );
			string objectPath = AssetDatabase.GetAssetPath( instance.RootTransform );

			// Find Path next to prefab
			if( string.IsNullOrEmpty( objectPath ) )
			{
				objectPath = AssetDatabase.GetAssetPath( PrefabUtility.GetCorrespondingObjectFromSource( instance.RootTransform ) );
			}

			Preferences.GlobalRelativeFolder = EditorPrefs.GetString( Preferences.PrefGlobalRelativeFolder, "" );
			string fullpath = string.Empty;
			string suggestedRelativePath = directory + objectPath;
			if( string.IsNullOrEmpty( objectPath ) )
				suggestedRelativePath = Application.dataPath;
			else
				suggestedRelativePath = Path.GetDirectoryName( suggestedRelativePath ).Replace( "\\", "/" );

			Preferences.GlobalFolder = EditorPrefs.GetString( Preferences.PrefGlobalFolder, "" );

			// Find best match
			if( Preferences.GlobalDefaultMode && AssetDatabase.IsValidFolder( Preferences.GlobalFolder.TrimStart( '/' ) ) )
				fullpath = directory + Preferences.GlobalFolder;
			else if( AssetDatabase.IsValidFolder( FileUtil.GetProjectRelativePath( suggestedRelativePath + Preferences.GlobalRelativeFolder ).TrimEnd( '/' ) ) )
				fullpath = suggestedRelativePath + Preferences.GlobalRelativeFolder;
			else if( AssetDatabase.IsValidFolder( FileUtil.GetProjectRelativePath( suggestedRelativePath ).TrimEnd( '/' ) ) )
				fullpath = suggestedRelativePath;
			else
				fullpath = Application.dataPath;

			string fileName = instance.name + "_Impostor";
			if( !string.IsNullOrEmpty( instance.m_impostorName ) )
				fileName = instance.m_impostorName;

			//Debug.Log( fullpath );
			//Debug.Log( fileName );

			string folderpath = EditorUtility.SaveFilePanelInProject( "Save Impostor to folder", fileName, "asset", "", FileUtil.GetProjectRelativePath( fullpath ) );
			fileName = Path.GetFileNameWithoutExtension( folderpath );

			if( !string.IsNullOrEmpty( fileName ) )
			{
				folderpath = Path.GetDirectoryName( folderpath ).Replace( "\\", "/" );
				if( !string.IsNullOrEmpty( folderpath ) )
				{
					folderpath += "/";
					if( !Preferences.GlobalDefaultMode )
					{
						instance.m_folderPath = folderpath;
					}
					else
					{
						Preferences.GlobalFolder = folderpath;
						EditorPrefs.SetString( Preferences.PrefGlobalFolder, Preferences.GlobalFolder );
					}
					instance.m_impostorName = fileName;
				}
			}
			return folderpath;
		}
#endif
	}
}
