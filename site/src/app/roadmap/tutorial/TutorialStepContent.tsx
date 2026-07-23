'use client';

import { useTour } from '@reactour/tour';
import Image, { StaticImageData } from 'next/image';
import { CSSProperties, ReactNode } from 'react';

import { TutorialButtonAction, TutorialVariant } from './tutorialConfig';
import { TutorialMascotPosition } from './tutorialMascots';

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

// reusable component for steps
export function TutorialStepContent({
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
