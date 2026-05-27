'use client';

import { StepType } from '@reactour/tour';
import { useTour } from '@reactour/tour';
import React, { ReactNode } from 'react';
import { basePopoverStyle } from './AppTourProvider';

const ROADMAP_TOUR_HAS_RUN_KEY = 'roadmap__tutorial_has_run';

enum TourStepName {
  welcome = 'welcome',
  year = 'year',
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
type TutorialVariant = 'welcome' | 'step';

export const variantPopoverStyle: Record<TutorialVariant, React.CSSProperties> = {
  welcome: {
    width: 366,
    height: 240,
    borderRadius: 13,
    border: '2px solid rgba(184, 184, 184, 0.5)',
    padding: '28px 24px 42px 24px',
    gap: 10,
    boxSizing: 'border-box',
  },
  step: {
    borderRadius: 10,
    border: '1px solid rgba(184, 184, 184, 0.5)',
    padding: '28px 40px 30px 40px',
    boxSizing: 'border-box',
  },
};

interface TutorialStepContentProps {
  variant?: TutorialVariant;
  title?: ReactNode;
  description: ReactNode;
  primaryLabel: string;
  primaryAction?: TutorialButtonAction;
  secondaryLabel?: string;
  secondaryAction?: TutorialButtonAction;
  onPrimaryClick?: () => void;
  onSecondaryClick?: () => void;
}

function TutorialStepContent({
  variant = 'step',
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
    padding: '8px 22px',
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
    boxShadow: `
      0px 3px 1px -2px rgba(0, 0, 0, 0.2),
      0px 2px 2px 0px rgba(0, 0, 0, 0.14),
      0px 1px 5px 0px rgba(0, 0, 0, 0.12)
    `,
  };

  const isWelcome = variant === 'welcome';

  return (
    <div style={{ textAlign: 'center', display: 'flex', flexDirection: 'column', gap: 10, height: '100%' }}>
      {title && <h2 style={{ margin: 0, fontSize: 30, fontWeight: 800 }}>{title}</h2>}
      <p
        style={{
          margin: 0,
          fontSize: 20,
          fontWeight: 500,
          color: isWelcome ? 'rgba(184, 184, 184, 1)' : 'rgba(255, 255, 255, 1)',
          flexGrow: 1,
        }}
      >
        {description}
      </p>
      <div
        style={{
          display: 'flex',
          justifyContent: 'center',
          gap: isWelcome ? 18 : 40,
        }}
      >
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
      selector: '#nonexistent',
      position: 'center',
      styles: {
        popover: (base) => ({ ...base, ...basePopoverStyle, ...variantPopoverStyle.welcome }),
      },
      content: (
        <TutorialStepContent
          variant="welcome"
          title="Welcome to Planner!"
          description={
            <>
              Would you like Peter to
              <br />
              show you around?
            </>
          }
          primaryLabel="YES"
          primaryAction="next"
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

    year: {
      selector: '#nonexistent',
      position: 'center',
      styles: {
        popover: (base) => ({ ...base, ...basePopoverStyle, ...variantPopoverStyle.step }),
      },
      content: (
        <TutorialStepContent
          variant="step"
          description={
            <>
              Is this your 1st year at
              <br />
              UCI?
            </>
          }
          primaryLabel="YES"
          primaryAction="next"
          secondaryLabel="NO"
          secondaryAction="close"
        />
      ),
    },
  };
}

export function stepsFactory(): Array<StepType> {
  const namedSteps = namedStepsFactory();
  return tourStepNames.map((key: TourStepName) => namedSteps[key]);
}
