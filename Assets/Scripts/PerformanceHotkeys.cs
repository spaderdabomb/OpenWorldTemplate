using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class PerformanceHotkeys : MonoBehaviour
{
    [SerializeField] private Camera camera1;
    [SerializeField] private Camera camera2;

    [SerializeField] private Terrain terrain;

    private bool _currentValue = false;

    // Update is called once per frame
    void Update()
    {
        if (Input.GetKeyDown(KeyCode.Alpha1))
        {
            _currentValue = !_currentValue;
            if (_currentValue)
            {
                camera1.gameObject.SetActive(true);
                camera2.gameObject.SetActive(false);
                Debug.LogError("Off");
            }
            else
            {
                camera1.gameObject.SetActive(false);
                camera2.gameObject.SetActive(true);
                Debug.LogError("On");
            }
        }

        if (Input.GetKeyDown(KeyCode.Alpha2))
        {
            terrain.drawTreesAndFoliage = !terrain.drawTreesAndFoliage;
        }
    }
}
