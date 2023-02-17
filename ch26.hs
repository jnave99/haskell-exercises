import Control.Monad 

newtype EitherT e m a =
    EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where 
    fmap f (EitherT ema) = EitherT ((fmap . fmap) f ema)

instance Applicative m => Applicative (EitherT e m) where 
    pure a = EitherT (pure (Right a))
    EitherT emf <*> EitherT ema = 
        EitherT $ (<*>) <$> emf <*> ema

instance Monad m => Monad (EitherT e m) where 
    return = pure 
    (EitherT ema) >>= f = 
        EitherT $ do 
            v <- ema 
            case v of 
                (Left a) -> return (Left a)
                (Right a) -> runEitherT (f a)

swapEither :: Either a b -> Either b a 
swapEither (Left a) = Right a
swapEither (Right a) = Left a

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e 
swapEitherT (EitherT ema) = EitherT $ (fmap swapEither ema)

newtype StateT s m a =
    StateT { runStateT :: s -> m (a,s) }

instance (Functor m) => Functor (StateT s m) where
    fmap f m = (fmap f m)
 
instance (Monad m) => Applicative (StateT s m) where 
    pure a = StateT (\s -> (pure (a, s)))
    (StateT smf) <*> (StateT sma) = StateT stateFunc
        where 
            stateFunc s = do
                fstResult <- (smf s)
                sndResult <- (sma (snd fstResult))
                return ((fst fstResult) (fst sndResult), snd sndResult)
                    
instance (Monad m) => Monad (StateT s m) where 
    return = pure 
    (StateT sma) >>= f = 
        StateT $ \s -> do 
            a <- sma s 
            runStateT (f (fst a)) s