'use client';

import { StepType } from '@reactour/tour';
import { useTour } from '@reactour/tour';
import Image, { StaticImageData } from 'next/image';
import React, { CSSProperties, ReactNode } from 'react';

import { basePopoverStyle } from './AppTourProvider';
import { TutorialMascotPosition, tutorialMascots } from './tutorialMascots';

const ROADMAP_TOUR_HAS_RUN_KEY = 'roadmap__tutorial_has_run';
const TUTORIAL_OUTLINE = 'tutorial-highlight-outline';

enum TourStepName {
  welcome = 'welcome',
  year = 'year',
  firstYearTransfer = 'firstYearTransfer',
  nonFirstYearImport = 'nonFirstYearImport',
}

type TutorialBranch = 'firstYear' | 'nonFirstYear';

export const TOUR_STEP = {
  welcome: 0,
  year: 1,
  firstYearTransfer: 2,
  nonFirstYearImport: 3,
} as const;

function setLocalStorageTourHasRun(value: 'true' | 'false') {
  if (typeof window === 'undefined') return;
  localStorage.setItem(ROADMAP_TOUR_HAS_RUN_KEY, value);
}

function markTourHasRun() {
  setLocalStorageTourHasRun('true');
}

function addTutorialOutline(selector: string) {
  document.querySelector(selector)?.classList.add(TUTORIAL_OUTLINE);
}
function removeTutorialOutline(selector: string) {
  document.querySelector(selector)?.classList.remove(TUTORIAL_OUTLINE);
}

// function getLocalStorageTourHasRun() {
//   if (typeof window === 'undefined') return null;
//   return localStorage.getItem(ROADMAP_TOUR_HAS_RUN_KEY);
// }

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
    overflow: 'visible',
  },
  step: {
    borderRadius: 10,
    border: '1px solid rgba(184, 184, 184, 0.5)',
    padding: '28px 40px 30px 40px',
    boxSizing: 'border-box',
    overflow: 'visible',
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
  mascot?: StaticImageData;
  mascotPosition?: TutorialMascotPosition;
  mascotStyle?: CSSProperties;
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
  mascot,
  mascotPosition = 'top-center',
  mascotStyle,
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

  const isWelcome = variant === 'welcome';

  return (
    <div className="tutorial-step-content">
      {mascot && (
        <Image
          src={mascot}
          alt="Peter the anteater"
          className={`tutorial-mascot tutorial-mascot--${mascotPosition}`}
          style={mascotStyle}
          width={mascot.width}
          height={mascot.height}
        />
      )}
      <div className="tutorial-step-body">
        {title && <h2 className="tutorial-step-title">{title}</h2>}
        <p className={`tutorial-step-description tutorial-step-description--${variant}`}>{description}</p>
        <div className={`tutorial-step-buttons ${isWelcome ? '' : 'tutorial-step-buttons--step'}`}>
          <button
            type="button"
            className="tutorial-step-button"
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
              className="tutorial-step-button"
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
    </div>
  );
}

function YearBranchStepContent() {
  const { setCurrentStep } = useTour();

  const chooseBranch = (branch: TutorialBranch) => {
    if (!setCurrentStep) return;
    setCurrentStep(branch === 'firstYear' ? TOUR_STEP.firstYearTransfer : TOUR_STEP.nonFirstYearImport);
  };

  return (
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
      secondaryLabel="NO"
      mascot={tutorialMascots.thinking}
      mascotPosition="bottom-right"
      onPrimaryClick={() => chooseBranch('firstYear')}
      onSecondaryClick={() => chooseBranch('nonFirstYear')}
    />
  );
}

function FirstYearBranchStepContent() {
  const { setCurrentStep } = useTour();

  const returnToBranchQuestion = () => {
    if (!setCurrentStep) return;
    setCurrentStep(TOUR_STEP.year);
  };

  return (
    <TutorialStepContent
      variant="step"
      description={
        <>
          If you have any transfer
          <br />
          credits, add them here!
        </>
      }
      primaryLabel="BACK"
      onPrimaryClick={returnToBranchQuestion}
      secondaryLabel="NEXT"
      secondaryAction="close"
      mascot={tutorialMascots.teach}
      mascotPosition="top-left"
    />
  );
}

function NonFirstYearBranchStepContent() {
  const { setCurrentStep } = useTour();

  const returnToBranchQuestion = () => {
    if (!setCurrentStep) return;
    setCurrentStep(TOUR_STEP.year);
  };

  return (
    <TutorialStepContent
      variant="step"
      description={
        <>
          Would you like to import
          <br />
          your previous year?
        </>
      }
      primaryLabel="BACK" // should be yes
      onPrimaryClick={returnToBranchQuestion} // should jump to import branch
      secondaryLabel="NEXT" // should be no
      secondaryAction="close" // should be next
      mascot={tutorialMascots.thinking}
      mascotPosition="bottom-left"
    />
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
          mascot={tutorialMascots.over}
          mascotPosition="top-center"
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
      content: <YearBranchStepContent />,
    },
    firstYearTransfer: {
      selector: '#credits-label',
      position: ({ right, top, height }) => [right + 16, top + height - 16],
      action: () => addTutorialOutline('#credits-label'),
      actionAfter: () => removeTutorialOutline('#credits-label'),
      styles: {
        popover: (base) => ({ ...base, ...basePopoverStyle, ...variantPopoverStyle.step }),
      },
      content: <FirstYearBranchStepContent />,
    },
    nonFirstYearImport: {
      selector: '#nonexistent',
      position: 'center',
      styles: {
        popover: (base) => ({ ...base, ...basePopoverStyle, ...variantPopoverStyle.step }),
      },
      content: <NonFirstYearBranchStepContent />,
    },
  };
}

export function stepsFactory(): Array<StepType> {
  const namedSteps = namedStepsFactory();
  return [namedSteps.welcome, namedSteps.year, namedSteps.firstYearTransfer, namedSteps.nonFirstYearImport];
}
