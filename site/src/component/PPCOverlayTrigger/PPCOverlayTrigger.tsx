/** temporary file/patch for overlay trigger to make it so user can mouse into it wihtout it closing */

import { FC, useState } from 'react';
import { OverlayTrigger, OverlayTriggerProps, Popover } from 'react-bootstrap';

interface PPCOverlayTriggerProps extends Omit<OverlayTriggerProps, 'overlay'> {
  popoverContent: React.ReactNode;
  children: React.ReactElement;
  popupListener?: (open: boolean) => void;
  setAllowSecondaryTap?: (allow: boolean) => void;
  disabled?: boolean;
}

const PPCOverlayTrigger: FC<PPCOverlayTriggerProps> = (props) => {
  const { setAllowSecondaryTap, popupListener, popoverContent, children, disabled, ...passedProps } = props;
  const [showInfoPopover, setShowInfoPopover] = useState(false);
  const [showPopoverTimeout, setShowPopoverTimeout] = useState(0);

  const POPOVER_DELAY = 80;
  const TOUCH_DELAY = 120;

  const showPopover = () => {
    setShowInfoPopover(true);
    popupListener?.(true);
    clearTimeout(showPopoverTimeout);
    setShowPopoverTimeout(0);
    setTimeout(() => setAllowSecondaryTap?.(true), TOUCH_DELAY);
  };

  const hidePopover = () => {
    setShowInfoPopover(false);
    setAllowSecondaryTap?.(false);
    popupListener?.(false);
    clearTimeout(showPopoverTimeout);
    setShowPopoverTimeout(0);
  };

  const handleMouseMove = () => {
    if (!showPopoverTimeout) return;
    clearTimeout(showPopoverTimeout);
    setShowPopoverTimeout(window.setTimeout(showPopover, POPOVER_DELAY));
  };

  const handleHover = () => {
    if (document.querySelector('.course.sortable-fallback')) return;
    if (disabled) return;
    clearTimeout(showPopoverTimeout);
    setShowPopoverTimeout(window.setTimeout(showPopover, POPOVER_DELAY));
  };

  const handleUnhover = (event: React.MouseEvent) => {
    try {
      const inTooltip = document.querySelector('.ppc-popover')?.contains(event?.relatedTarget as HTMLElement);
      if (!inTooltip) hidePopover();
    } catch {
      hidePopover();
    }
  };

  const popover = (
    <Popover className="ppc-popover" id="ppc-popover" onMouseLeave={hidePopover}>
      {popoverContent}
    </Popover>
  );

  return (
    <OverlayTrigger {...passedProps} show={showInfoPopover} overlay={popover}>
      <div onMouseEnter={handleHover} onMouseLeave={handleUnhover} onMouseMove={handleMouseMove}>
        {children}
      </div>
    </OverlayTrigger>
  );
};

export default PPCOverlayTrigger;
