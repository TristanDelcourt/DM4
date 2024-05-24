# malaxar is here
import pygame
import math

pygame.init()
screen_size = 500
window = pygame.display.set_mode((screen_size, screen_size))
pygame.display.set_caption("quoicoubeh")
clock = pygame.time.Clock()

G = 1


class Corps:

  def __init__(self, masse, color="yellow"):
    self.masse = masse
    self.color = color


def update():
  


run = True
while run:
  for event in pygame.event.get():
    if event.type == pygame.QUIT:
      run = False

  update()

  pygame.display.update()
