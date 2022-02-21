/* eslint-disable @typescript-eslint/ban-ts-comment */
// @ts-nocheck
import debounce from 'lodash.debounce';
import { useCallback, useRef } from 'react';

export const cardQuantityInputFieldDebounceTimeMs = 500;

const useDebouncedCallback = (callback, delay) => {
  const callbackRef = useRef();
  callbackRef.current = callback;
  return useCallback(
    debounce((...args) => callbackRef?.current?.(...args), delay), // https://github.com/microsoft/TypeScript/issues/41371
    []
  );
};

export default useDebouncedCallback;
