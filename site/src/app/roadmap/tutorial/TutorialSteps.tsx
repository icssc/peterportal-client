'use client';

import { useTour } from '@reactour/tour';
import { StepType } from '@reactour/tour';

import { basePopoverStyle } from './AppTourProvider';
import {
  TOUR_STEP,
  TourStepName,
  TutorialBranch,
  addTutorialOutline,
  markTourHasRun,
  removeTutorialOutline,
  variantPopoverStyle,
} from './tutorialConfig';
import { TutorialStepContent } from './TutorialStepContent';
import { tutorialMascots } from './tutorialMascots';

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

// Factories
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
      selector: '#nonexistent', // control the highlighted button
      position: 'center', // control the position of the popover
      styles: {
        popover: (base) => ({ ...base, ...basePopoverStyle, ...variantPopoverStyle.step }),
      },
      content: <YearBranchStepContent />, // popover content
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
