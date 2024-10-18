using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UIElements;

public class FPSTracker : MonoBehaviour
{
    [SerializeField] UIDocument fpsMainDocument;
    private Label fpsLabel;
    private float timeWindow = 1.0f;
    private float elapsedTime = 0f;
    private Queue<float> frameTimes = new Queue<float>(); // Stores the time of each frame
    private float averageFPS = 0.0f;

    void Start()
    {
        fpsLabel = fpsMainDocument.rootVisualElement.Q<Label>("FPSLabel");
        print(fpsLabel.text);
    }

    // Update is called once per frame
    void Update()
    {
        frameTimes.Enqueue(Time.deltaTime);
        elapsedTime += Time.deltaTime;

        // Remove frames from the queue if they are outside the time window
        while (elapsedTime > timeWindow)
        {
            elapsedTime -= frameTimes.Dequeue();
        }

        // Calculate the average FPS over the time window
        averageFPS = frameTimes.Count / elapsedTime;

        fpsLabel.text = $"FPS: {averageFPS.ToString("F1")}";
    }
}
