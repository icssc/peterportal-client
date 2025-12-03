'use client';
import React, { FC, ReactNode, useState } from 'react';
import { Popover } from '@mui/material';
import './PPCOverlay.scss';

interface PPCOverlayProps {
  popoverContent: ReactNode;
  children: React.ReactElement;
  popupListener?: (open: boolean) => void;
  disabled?: boolean;
  anchor: 'top' | 'bottom' | 'left' | 'right';
  transform: 'top' | 'bottom' | 'left' | 'right';
}

const anchorMap = {
  top: { vertical: 'top', horizontal: 'center' } as const,
  bottom: { vertical: 'bottom', horizontal: 'center' } as const,
  left: { vertical: 'center', horizontal: 'left' } as const,
  right: { vertical: 'center', horizontal: 'right' } as const,
};

const transformMap = {
  top: { vertical: 'bottom', horizontal: 'center' } as const,
  bottom: { vertical: 'top', horizontal: 'center' } as const,
  left: { vertical: 'center', horizontal: 'right' } as const,
  right: { vertical: 'center', horizontal: 'left' } as const,
};

const PPCOverlay: FC<PPCOverlayProps> = ({
  popoverContent,
  children,
  popupListener,
  disabled = false,
  anchor,
  transform,
}) => {
  const [timer, setTimer] = useState<ReturnType<typeof setTimeout> | null>(null);

  const [anchorEl, setAnchorEl] = useState<HTMLElement | null>(null);
  const open = Boolean(anchorEl);

  const showPopover = (event: React.MouseEvent<HTMLElement>) => {
    if (disabled) {
      return;
    }

    setAnchorEl(event.currentTarget);
    popupListener?.(true);
  };

  const hidePopover = () => {
    if (timer) clearTimeout(timer);
    setTimer(null);

    setAnchorEl(null);
    popupListener?.(false);
  };

  const handleUnhover = (e: React.MouseEvent) => {
    const relatedTarget = e.relatedTarget as Node | null;
    const popoverContent = document.querySelector('.ppc-popover-content');

    if (!popoverContent || !relatedTarget || !popoverContent.contains(relatedTarget)) {
      hidePopover();
    }
  };

  const clonedChild = React.cloneElement(children, {
    onMouseEnter: (e: React.MouseEvent<HTMLElement>) => {
      showPopover(e);
      children.props.onMouseEnter?.(e);
    },
    onMouseLeave: (e: React.MouseEvent<HTMLElement>) => {
      handleUnhover(e);
      children.props.onMouseLeave?.(e);
    },
  });

  return (
    <div className="ppc-popover-trigger">
      {clonedChild}
      <Popover
        open={open}
        anchorEl={anchorEl}
        onClose={hidePopover}
        anchorOrigin={anchorMap[anchor]}
        transformOrigin={transformMap[transform]}
        slotProps={{
          paper: {
            className: 'ppc-popover-content',
            onMouseLeave: hidePopover,
            sx: {
              pointerEvents: 'auto',
            },
          },
        }}
        sx={{
          pointerEvents: 'none',
        }}
      >
        {popoverContent}
      </Popover>
    </div>
  );
};

export default PPCOverlay;
