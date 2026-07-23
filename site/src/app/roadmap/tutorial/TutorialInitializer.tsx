'use client';

import { useTour } from '@reactour/tour';
import { useEffect } from 'react';
import { stepsFactory } from './TutorialSteps';
import { tourShouldRun } from './tutorialConfig';

export function TutorialInitializer() {
  const { setSteps, setCurrentStep, setIsOpen } = useTour();

  useEffect(() => {
    if (!setSteps || !setCurrentStep) return;
    setSteps(stepsFactory());

    if (tourShouldRun()) {
      setCurrentStep(0);
      setIsOpen?.(true);
    }
  }, [setCurrentStep, setIsOpen, setSteps]);

  return null;
}
