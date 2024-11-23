import { createSlice, PayloadAction } from '@reduxjs/toolkit';
import { RootState } from '../store';

const initialState: { containerItems: Record<string, string[]>; fromContainer?: string } = { containerItems: {} };

export const dndSlice = createSlice({
  name: 'dnd',
  initialState,
  reducers: {
    setContainerItems(state, action: PayloadAction<{ id: string; items: string[] }>) {
      const { id, items } = action.payload;
      state.containerItems[id] = items;
    },
    moveToContainer(state, action: PayloadAction<{ oldContainerId: string; newContainerId: string; itemId: string }>) {
      const { oldContainerId, newContainerId, itemId } = action.payload;
      state.containerItems[oldContainerId] = state.containerItems[oldContainerId].filter((id) => id !== itemId);
      if (!state.containerItems[newContainerId].includes(itemId)) {
        state.containerItems[newContainerId].push(itemId);
      }
      console.log('moved');
    },
    setFromContainer(state, action: PayloadAction<{ itemId?: string }>) {
      const { itemId } = action.payload;
      if (!itemId) {
        state.fromContainer = undefined;
        return;
      }
      state.fromContainer = Object.keys(state.containerItems).find((containerId) =>
        state.containerItems[containerId].includes(itemId),
      );
      console.log('fromContainer', state.fromContainer);
    },
  },
});

export const { setContainerItems, moveToContainer, setFromContainer } = dndSlice.actions;

export const selectDnd = (state: RootState) => state.dnd;

export default dndSlice.reducer;
