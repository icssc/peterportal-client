'use client';

import { StepType } from '@reactour/tour';
import { useTour } from '@reactour/tour';
import { ReactNode } from 'react';

const ROADMAP_TOUR_HAS_RUN_KEY = 'roadmap__tutorial_has_run';

enum TourStepName {
  welcome = 'welcome',
}

const tourStepNames = Object.values(TourStepName);

function setLocalStorageTourHasRun(value: 'true' | 'false') {
  if (typeof window === 'undefined') return;
  localStorage.setItem(ROADMAP_TOUR_HAS_RUN_KEY, value);
}

// function getLocalStorageTourHasRun() {
//   if (typeof window === 'undefined') return null;
//   return localStorage.getItem(ROADMAP_TOUR_HAS_RUN_KEY);
// }

function markTourHasRun() {
  setLocalStorageTourHasRun('true');
}

type TutorialButtonAction = 'none' | 'close' | 'next' | 'back';

interface TutorialStepContentProps {
  title: ReactNode;
  description: ReactNode;
  primaryLabel: string;
  primaryAction?: TutorialButtonAction;
  secondaryLabel?: string;
  secondaryAction?: TutorialButtonAction;
  onPrimaryClick?: () => void;
  onSecondaryClick?: () => void;
}

function TutorialStepContent({
  title,
  description,
  primaryLabel,
  primaryAction = 'none',
  secondaryLabel,
  secondaryAction = 'none',
  onPrimaryClick,
  onSecondaryClick,
}: TutorialStepContentProps) {
  const { currentStep, setCurrentStep, setIsOpen } = useTour();

  const runAction = (action: TutorialButtonAction) => {
    if (action === 'none') return;
    if (action === 'close') {
      setIsOpen?.(false);
      return;
    }
    if (currentStep == null || !setCurrentStep) return;
    if (action === 'next') setCurrentStep(currentStep + 1);
    if (action === 'back') setCurrentStep(Math.max(0, currentStep - 1));
  };

  const buttonStyle = {
    border: 'none',
    borderRadius: 8,
    padding: '9px 18px',
    fontFamily: 'Roboto, sans-serif',
    fontWeight: 500,
    fontStyle: 'normal',
    fontSize: '0.9375rem',
    lineHeight: '26px',
    letterSpacing: '0.4px',
    textTransform: 'uppercase' as const,
    color: '#ffffff',
    background: 'var(--mui-palette-primary-main)',
    cursor: 'pointer',
  };

  return (
    <div style={{ textAlign: 'center' }}>
      <h2 style={{ margin: 0, fontSize: 30, fontWeight: 800 }}>{title}</h2>
      <p style={{ marginTop: 12, marginBottom: 24, fontSize: 20, fontWeight: 500, color: 'rgba(184, 184, 184, 0.5)' }}>
        {description}
      </p>
      <div style={{ display: 'flex', justifyContent: 'center', gap: 18 }}>
        <button
          type="button"
          style={buttonStyle}
          onClick={() => {
            onPrimaryClick?.();
            runAction(primaryAction);
          }}
        >
          {primaryLabel}
        </button>
        {secondaryLabel && (
          <button
            type="button"
            style={buttonStyle}
            onClick={() => {
              onSecondaryClick?.();
              runAction(secondaryAction);
            }}
          >
            {secondaryLabel}
          </button>
        )}
      </div>
    </div>
  );
}

export function tourShouldRun(): boolean {
  if (typeof window === 'undefined') return false;
  return !(
    // commented out for testing purposes

    // getLocalStorageTourHasRun() === 'true' ||
    window.matchMedia('(max-width: 799px)').matches
  );
}

function namedStepsFactory(): Record<TourStepName, StepType> {
  return {
    welcome: {
      selector: '#nonexistent', // no selector so dims everything
      position: 'center',
      content: (
        <TutorialStepContent
          title="Welcome to Planner!"
          description={
            <>
              Would you like Peter to
              <br />
              show you around?
            </>
          }
          primaryLabel="YES"
          primaryAction="close"
          secondaryLabel="NO"
          secondaryAction="close"
          onPrimaryClick={markTourHasRun}
          onSecondaryClick={markTourHasRun}
        />
      ),
      actionAfter: () => {
        markTourHasRun();
      },
    },
  };
}

export function stepsFactory(): Array<StepType> {
  const namedSteps = namedStepsFactory();
  return tourStepNames.map((key: TourStepName) => namedSteps[key]);
}
