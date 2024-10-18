using UnityEngine;
using UnityEditor;

public class TerrainPrefabPlacer : EditorWindow
{
    public GameObject prefab;           // The prefab to place on the terrain
    public Terrain terrain;             // The terrain on which to place objects
    public GameObject treeHolder;
    public int numberOfObjects = 100;   // The number of objects to place
    public float minHeight = 0;         // Minimum terrain height for placement (0 = lowest point of terrain)
    public float maxHeight = 1;         // Maximum terrain height for placement (1 = highest point of terrain)

    [MenuItem("Tools/Prefab Placer")]
    public static void ShowWindow()
    {
        GetWindow<TerrainPrefabPlacer>("Prefab Placer");
    }

    private void OnGUI()
    {
        GUILayout.Label("Prefab Placement Settings", EditorStyles.boldLabel);

        prefab = (GameObject)EditorGUILayout.ObjectField("Prefab", prefab, typeof(GameObject), false);
        terrain = (Terrain)EditorGUILayout.ObjectField("Terrain", terrain, typeof(Terrain), true);
        treeHolder = (GameObject)EditorGUILayout.ObjectField("TreeHolder", treeHolder, typeof(GameObject), true);
        numberOfObjects = EditorGUILayout.IntField("Number of Objects", numberOfObjects);
        minHeight = EditorGUILayout.Slider("Min Height (0-1)", minHeight, 0, 1);
        maxHeight = EditorGUILayout.Slider("Max Height (0-1)", maxHeight, 0, 1);

        if (GUILayout.Button("Place Prefabs"))
        {
            PlacePrefabs();
        }
    }

    private void PlacePrefabs()
    {
        if (terrain == null || prefab == null)
        {
            Debug.LogError("Terrain or Prefab is not set!");
            return;
        }

        TerrainData terrainData = terrain.terrainData;
        Vector3 terrainPos = terrain.transform.position;
        Vector3 terrainSize = terrainData.size;

        for (int i = 0; i < numberOfObjects; i++)
        {
            // Generate random X and Z positions within the terrain's bounds
            float randomX = Random.Range(0, terrainSize.x);
            float randomZ = Random.Range(0, terrainSize.z);

            // Get the terrain height at the generated X, Z positions
            float terrainHeight = terrainData.GetHeight(
                Mathf.FloorToInt((randomX / terrainSize.x) * terrainData.heightmapResolution),
                Mathf.FloorToInt((randomZ / terrainSize.z) * terrainData.heightmapResolution)
            );

            // Adjust the height according to the terrain's position in the world
            float yPos = terrainHeight + terrainPos.y;

            // Check height range to avoid placing prefabs in undesired regions
            if (yPos >= terrainPos.y + (minHeight * terrainSize.y) && yPos <= terrainPos.y + (maxHeight * terrainSize.y))
            {
                // Place the prefab at the calculated position on the terrain
                Vector3 position = new Vector3(randomX + terrainPos.x, yPos, randomZ + terrainPos.z);
                Instantiate(prefab, position, Quaternion.identity, treeHolder.transform);
            }
        }

        Debug.Log($"Placed {numberOfObjects} objects on the terrain.");
    }
}
