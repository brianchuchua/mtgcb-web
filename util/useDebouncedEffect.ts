import { useCallback, useEffect } from 'react';

interface UseDebouncedEffectFunction {
  (effect: () => void, delay: number, deps: Array<unknown>): void;
}

const useDebouncedEffect: UseDebouncedEffectFunction = (effect, delay, deps): void => {
  // eslint-disable-next-line react-hooks/exhaustive-deps
  const callback = useCallback(effect, deps);

  useEffect(() => {
    const handler = setTimeout(() => {
      callback();
    }, delay);

    return () => {
      clearTimeout(handler);
    };
  }, [callback, delay]);
};

export default useDebouncedEffect;
