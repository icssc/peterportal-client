'use client';
import React, { FC, ReactNode, useState } from 'react';
import { Popover, Box } from '@mui/material';
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
  const [anchorEl, setAnchorEl] = useState<HTMLElement | null>(null);
  const [open, setOpen] = useState(false);
  const [timer, setTimer] = useState<ReturnType<typeof setTimeout> | null>(null);

  const showPopover = (event: React.MouseEvent<HTMLDivElement, MouseEvent>) => {
    setAnchorEl(event.currentTarget);
    setOpen(true);
  };

  const hidePopover = () => {
    if (timer) clearTimeout(timer);
    setTimer(null);
    setOpen(false);
    setAnchorEl(null);
    popupListener?.(false);
  };

  const handleHover = (e: React.MouseEvent<HTMLDivElement, MouseEvent>) => {
    if (disabled) return;
    showPopover(e);
  };

  const handleUnhover = (e: React.MouseEvent) => {
    try {
      const inTooltip = document.querySelector('.ppc-popover-content')?.contains(e?.relatedTarget as HTMLElement);
      if (!inTooltip) hidePopover();
    } catch {
      hidePopover();
    }
  };

  return (
    <Box className="ppc-popover-trigger" onMouseEnter={handleHover} onMouseLeave={handleUnhover}>
      {children}
      <Popover
        open={open}
        anchorEl={anchorEl}
        onClose={hidePopover}
        anchorOrigin={anchorMap[anchor]}
        transformOrigin={transformMap[transform]}
        slotProps={{
          paper: {
            className: 'ppc-popover-content',
            onMouseEnter: () => {
              popupListener?.(true);
            },
            onMouseLeave: () => {
              hidePopover();
            },
            sx: {
              pointerEvents: 'auto',
            },
          },
        }}
      >
        {popoverContent}
      </Popover>
    </Box>
  );
};

export default PPCOverlay;
